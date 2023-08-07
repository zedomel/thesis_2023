library(rdatacite)
library(dataone)
library(httr)
library(jsonlite)
library(ggplot2)
library(forcats)
library(dplyr)
library(viridis)
library(rgdal)
library(rworldmap)
library(httr)
library(tidygeocoder)
library(sf)
library(cartography)

#### Auxiliary functions ####

address <- latlon %>%
  filter(!is.na(lat) & !is.na(long)) %>%
  reverse_geocode(lat = lat, long = long, method = 'osm', address = address_found, full_results = TRUE)

country.count <- address %>% 
  filter(!is.na(country_code)) %>% 
  count(country_code) %>% 
  mutate(name=countrycode::countrycode(toupper(country_code), origin = 'iso2c', destination = 'country.name.en'))

mutate(lat = if(is.na(lat) && 'geoLocationPlace' %in% colnames(.)) pull(geo(paste(na.omit(geoLocationPlace), sep=' ', collapse = ' '), method = 'osm'),'lat') else lat) 
  mutate(lon = if(is.na(lon) && 'geoLocationPlace' %in% colnames(.)) pull(geo(paste(na.omit(geoLocationPlace), sep=' ', collapse = ' '), method = 'osm'),'long') else lon)
geoLocation2LatLon <- function(geoLocation) {
  if (is_empty(geoLocation)) {
    return(as_tibble(data.frame(lat=NA, lon=NA)))
  }
  
  geoLocation %>% 
    mutate(lat = if("geoLocationPoint" %in% colnames(.)) na.omit(pull(geoLocationPoint, 'pointLatitude')) else NA) %>%
    mutate(lon = if("geoLocationPoint" %in% colnames(.)) na.omit(pull(geoLocationPoint, 'pointLongitude')) else NA) %>%
    mutate(lat = if(is.na(lat) && 'geoLocationPlace' %in% colnames(.)) pull(geo(na.omit(geoLocationPlace), method = 'osm'),'lat') else lat) %>%
    mutate(lon = if(is.na(lon) && 'geoLocationPlace' %in% colnames(.)) pull(geo(na.omit(geoLocationPlace), method = 'osm'),'long') else lon)
}

g <- data.cite$attributes$geoLocations[[50]]
g %>%
  mutate(lat = last(na.omit(geoLocationPlace))

)
#### DATA CITE Datasets ####

# List of terms to search
terms <- c('biotic', 'ecological', 'inter(-?)specific', 'community', 'biological', 'species')
raw_query = '(titles.title:"%s%%20interaction"%%20OR%%20descriptions.description:"%s%%20interaction")%%20AND%%20types.resourceTypeGeneral:Dataset'
base_url <- 'https://api.datacite.org/dois?query='

# Iterate over terms and search datacite for datasets
data.cite <- NULL
for(term in terms) {
  query <- sprintf(raw_query, term, term)
  url = paste0(base_url, query)
  
  # Pagination
  repeat {
    r <- httr::GET(url = url)
    tmp <- fromJSON(httr::content(r, as = 'text'))
    tmp$data <- tibble::as_tibble(tmp$data)
    tmp$data$attributes$term <- paste(term, 'interaction', sep = ' ')
    data.cite <- bind_rows(data.cite, tmp$data)
    
    # If null, stop pagination
    if (is.null(tmp$links$`next`)) {
      break
    }
    
    url = tmp$links$`next`
    # Wait 1 second to not be blocked by datacite
    Sys.sleep(1)
  }
}

#Save results
save(data.cite, file = 'datacite-datasets.RData')
# Clean env.
rm(tmp, query, url, r)

###### DATA ONE Datasets #######

# Production KNB node
cn <- CNode("PROD")
mn <- getMNode(cn, "urn:node:KNB")

# List of search terms
terms <- c('biotic', 'ecological', 'inter-specific', 'interspecific', 'community', 'biological', 'species')
data.one <- NULL

#Iterate over terms and search dataone for datasets
for (term in terms) {
  query <- sprintf("title:\"%1$s interaction\" OR title:\"%1$s interactions\" OR abstract:\"%1$s interaction\" OR abstract:\"%1$s interactions\" OR keywords:\"%1$s interaction\" OR keywords:\"%1$s interactions\"", term)
  query.terms <- list(q=query,
                      rows=1000,
                      sort="dateUploaded+desc")
  result <- query(mn, solrQuery=query.terms, as="data.frame")
  if (length(result)) {
    result$term <- paste(term, 'interaction', sep = ' ')
    data.one <- bind_rows(data.one, result)
  }
}

#Save results
save(data.one, file = 'dataone-datasets.RData')
#Clean env.
rm(result, query, query.terms, mn, cn)

#### MERGE RESULTS ####

#Load results
load(file = 'datacite-datasets.RData')
load(file = 'dataone-datasets.RData')

# Convert descriptions to a tibble (df)
descriptions <- lapply(data.cite$attributes$descriptions, '[[', 'description')
descriptions[sapply(descriptions, is.null)] <- c(NA)
descriptions <- as_tibble(stringi::stri_join_list(descriptions, sep='.'))
descriptions$doi <- data.cite$attributes$doi

# Select columns and join descriptions by DOI from DataCite source
data.cite2 <- as_tibble(data.cite$attributes) %>%
  unnest(c(titles)) %>%
  left_join(descriptions, by=c("doi"="doi"), multiple='all') %>%
  distinct(doi, .keep_all=T)  %>%
  mutate(publicationYear=as.integer(publicationYear)) %>%
  mutate(keywords=lapply(subjects, '[[', 'subject')) %>%
  select(id=doi,title,keywords,publisher,term, abstract=value, publicationYear)

# Select columns from DataOne source

data.one <- as_tibble(data.one)
data.one2 <- data.one %>%
  mutate(publicationYear = 
           if_else(
             !is.na(datePublished),
             as.integer(format(as.Date(datePublished), '%Y')),
             as.integer(format(as.Date(dateUploaded), '%Y')))) %>%
  mutate(publisher='DataONE') %>%
  select(id, abstract,keywords, title, term, publicationYear, publisher)

datasets <- bind_rows(data.cite2, data.one2)
rm(data.cite2,data.one2)

agg_year <- datasets%>%
  filter(!is.na(publicationYear)) %>%
  filter(publicationYear < 2023) %>%
  count(term,publicationYear)
agg_year  

py_plot <- ggplot(agg_year, aes(x=publicationYear, y=n, group=term, color=term)) +
  ggtitle("Number of datasets including the term “species interaction” and other similar terms throughout the years") +
  xlab('Year') +
  ylab('Number of datasets') +
  guides(color = guide_legend(title = "Term")) +
  geom_line()
py_plot

# Spatial analysis of datasets 

#Convert geoLocations from DateCite to lat/lon (point)
latlon <- bind_rows(lapply(data.cite$attributes$geoLocations, geoLocation2LatLon))
latlon <- latlon %>%
  select(lat,lon)

#Convert boudingbox from DataOne to lat/lon (center point)
latlon <- bind_rows(latlon, data.one %>%
  mutate(lon=rowMeans(select(., southBoundCoord, westBoundCoord)), lat = rowMeans(select(., northBoundCoord, eastBoundCoord))) %>%
  select(lat,lon))

address <- latlon %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  reverse_geocode(lat = lat, long = lon, method = 'osm', address = address_found, full_results = TRUE)


country.count <- address %>% 
  filter(!is.na(country_code)) %>% 
  count(country_code) %>% 
  mutate(name=countrycode::countrycode(toupper(country_code), origin = 'iso2c', destination = 'country.name.en'))
country.count

country.count$n <- as.numeric(country.count$n)
country.count %>%
  ggplot(aes(x=as.numeric(n))) +
  geom_histogram(bins=20, fill='#69b3a2', color='white')


country.count$country_code <- str_to_upper(country.count$country_code)
map <- st_read(dsn = 'data/TM_WORLD_BORDERS_SIMPL-0.3.shp', quiet = TRUE)
# Remove Antarctica
map <- map[map$ISO2 != 'AQ',]
map <- map %>%
  left_join(. , country.count, by=c("ISO2"="country_code"))
plot(st_geometry(map), col=NA, border=NA, bg="#aadaff")
choroLayer(
  x = map,
  var = "n",
  method = "geom",
  nclass = 7,
  col = carto.pal(pal1 = "orange.pal", n1=7,),
  border = "white",
  lwd=0.5,
  legend.pos = 'topright',
  legend.horiz = T,
  legend.title.txt = "Number of datasets per country",
  add = TRUE
)
layoutLayer(title="Number of dataset per country", 
            sources = 'Sources: DataCite and DataONE',
            author = paste0('cartography', packageVersion("cartography")),
            frame = FALSE, north = FALSE, tabtitle = TRUE, theme = "sand.pal")
north(pos="topleft")



# Dataset by publisher
publishers <- datasets %>%
  count(publisher) %>%
  filter(n > 9)

publishers <- rbind(publishers, datasets %>% 
    count(publisher) %>%
    filter(n < 10) %>%
    summarise(publisher='Other',n=sum(n)))

publishers %>%
  mutate(name=fct_reorder(publisher, n)) %>%
  ggplot(aes(x=name,y=n)) + 
  geom_bar(stat="identity") +
  labs(
    title="Number of datasets per Publisher"
  ) +
  coord_flip() +
  xlab("") +
  theme(legend.position = 'none')


#### KEYWORDS ANALYSIS ####
library(wordcloud2)
library(tidyr)
library(tidytext)
library(tidyverse)
library(tm)
library(SnowballC)


keywords <- datasets$keywords[lengths(datasets$keywords) != 0] %>%
  map_df(enframe, name = "index", .id = 'docid', value='keyword')

keywords$keyword <- gsub("[[:punct:]]", "", keywords$keyword) # Remove punctuation
keywords$keyword <- gsub("[[:digit:]]", "", keywords$keyword) # Remove numbers
keywords$keyword <- gsub("\\s+", " ", str_trim(keywords$keyword)) # Remove extra whitespaces
keywords$keyword <- gsub("FOS:?\\s+", "", keywords$keyword)
keywords <- keywords %>%
  mutate(keyword = str_to_lower(keyword)) %>%
  distinct(docid,keyword, .keep_all=T)
keywords
ndoc <- length(unique(keywords$docid))
maxFreq <- ndoc*0.5
minFreq <- ndoc*0.01

tokens <- keywords %>%
  filter(keyword != 'biological sciences') %>%
  unnest_tokens(word, keyword) %>%
  anti_join(stop_words) %>%
  mutate(word2=lemmatize_words(word)) %>%
  count(word2, sort=T) %>%
  filter(
    !(word2 %in% c("fos", "itex", "bdbcbbbcdeb", "science")) & 
      str_length(word2) > 3 & 
      str_length(word2) < 15 &
      n < maxFreq & n > minFreq
    
    ) %>%
  ungroup()
tokens

set.seed(1234) # for reproducibility 
wordcloud2(tokens %>% top_n(200), size=1, color='random-dark')  
  

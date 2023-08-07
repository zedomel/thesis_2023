#############################################################
############### Datasets Overview ###########################
#############################################################

#
# Description
# author: José Augusto Salim <joseasalim@usp.br>
# date: 2023-05-05
#

library(stringr)
library(dplyr)
library(plyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(shadowtext)
library(hrbrthemes)
library(viridis)

# Load COLORS
source('colors.R')

# Load functions
source('functions.R')

########## BEGIN ##############

# Terms to search
terms <- c("species", "biotic", "ecological", "community", "inter-specific", "interspecific", "biological")

# Datasets
data.list <- list(dryad=NULL, dataone=NULL,zenodo=NULL,figshare=NULL)
repositories <- c('dryad', 'dataone', 'zenodo', 'figshare')

# Search repositories for each term
for (term in terms) {
  print(term)
  
  for (repo in repositories) {
    print(paste("Repository", repo))
    file <- paste0('data/datasets/', repo,'-',term, '.rds')
    if(!file.exists(file)) {
      print("RDS file not found. Retrieving datasets from API...")
      data <- do.call(paste0('get_', repo, '_datasets'), args = list(term=term))
      print(paste("Saving to RDS file:", file))
      saveRDS(data, file=file)
      data.list[[repo]] <- bind_rows(data.list[[repo]], data)
    } else {
      print("RDS file found. Loading data...")
      aux <- readRDS(file)
      data.list[[repo]] <- data.list[[repo]] %>%
        bind_rows(aux)
    }  
  }
}

# Fix dataone date format
data.list[["dataone"]]$pubDate <- format.Date(data.list[["dataone"]]$pubDate, "%Y-%m-%d")

# Bind all datasets into one data.frame
# TODO: investigate data for more metadata (licence, persistent identifiers (multiple), standards, provenance, preservation policy)
data.list <- lapply(data.list, mutate, alternateId=as.character(alternateId))
data.all <- dplyr::bind_rows(data.list, .id="source")
dim(data.all)

# Metrics (citations, downloads, views)
#metrics <- get_datacite_metrics(data.all$id)
metrics <- readRDS('data/datasets/metrics.rds')
dim(metrics)
dim(data.all)

data.all <- data.all %>%
  bind_cols(metrics %>% select(-id) %>% dplyr::rename(c=citations,d=downloads,v=views)) %>%
  mutate(citations=citations+c, dowloads=downloads+d, views=views+v) %>%
  select(-c,-d,-v)


data.all <- data.all %>%
  arrange(id,desc(citations),desc(downloads),desc(views)) %>%
  distinct(source,id, .keep_all = T)
dim(data.all)

data.all %>%
  dplyr::count(source)

# Convert dates to year and merge inter-specific and interspecific
data.all <- data.all %>%
  dplyr::mutate(pubYear=as.integer(format(as.Date(pubDate),"%Y"))) %>%
  mutate(term=stringr::str_remove(term,"-"))

data.uniq <- data.all %>%
  arrange(title,pubYear,desc(citations),desc(downloads), desc(views)) %>%
  dplyr::distinct(title,pubYear, .keep_all = T)
dim(data.uniq)

data.one.removed <- read.csv(file='data/datasets/dataone-non-eml.tsv', sep = '\t', header = F) %>% as_tibble()
names(data.one.removed) <- c("id", "std")
data.one.removed <- data.one.removed %>%
  distinct(id, .keep_all = T) %>%
  filter(stringr::str_detect(std,"dryad"))
dim(data.one.removed)
head(data.one.removed)

data.one <- jsonlite::fromJSON("https://search.dataone.org/cn/v2/query/solr/?q=-obsoletedBy:*%20AND%20%20-formatId:*dataone.org%5C%2Fcollections*%20AND%20%20-formatId:*dataone.org%5C%2Fportals*%20AND%20formatType:METADATA&wt=json")
numFound <- data.one$response$numFound
numFound

data.one <- jsonlite::fromJSON("https://search.dataone.org/cn/v2/query/solr/?q=-obsoletedBy:*%20AND%20%20formatId:*datadryad.org*%20AND%20%20formatType:METADATA&wt=json")
numFoundDryad <- data.one$response$numFound
numFoundDryad

numFoundDryad/numFound*100
numFound - numFoundDryad

# Datasets by publication year
plt <- data.uniq %>%
  dplyr::filter(!is.na(pubYear)) %>%
  dplyr::distinct(title,pubYear, .keep_all = T) %>%
  dplyr::count(pubYear) %>%
  ggplot(aes(x=pubYear, y=cumsum(n))) +
  geom_point(size=3, shape=16, color='#076fa2') +
  geom_line(linewidth=1, color='#076fa2') + 
  coord_cartesian(clip = "off") +
  labs(title = "Cumulative number of datasets per year", subtitle = stringr::str_wrap('Accumlated number of biotic interaction datasets published from 2003 to 2023 in Dryad, DataONE, Figshare and Zenodo', 60)) +
  xlab("Year") +
  ylab('Number of datasets') +
  scale_x_continuous(breaks = seq(2003, 2023, 4)) +
  scale_y_continuous() +
  theme(
    plot.title = element_text(size=12, face='bold'),
    plot.subtitle = element_text(size=10),
    axis.title.y = element_text(size=12, face='bold'),
    axis.title.x = element_text(size=12, face='bold', margin = margin(10,0,0,0,'pt')),
    axis.text.x = element_text(size=10, angle = 30, hjust = 0.5, vjust = 0.5),
    axis.text.y = element_text(size=10),
  )
plt
ggsave('images/cum-datasets-year.png', plot = plt, dpi = 300)

# Number of datasets by interaction term
data.by.term <- data.uniq %>% 
  dplyr::count(term) %>%
  arrange(n) %>%
  mutate(term=factor(term, levels=term))

data.by.term
#Barplot

plt <- data.by.term %>% 
  ggplot() + 
  geom_col(aes(y=term,x=n), fill=BLUE, width=0.8) +
  scale_x_continuous(
    #limits=c(0, 86700), #TODO: fix limits
    #breaks = seq(0,max(metrics$value),1000), #TODO: fix breaks
    expand = c(0, 0),
    position = 'top',
    labels = scales::label_number(scale_cut = scales::cut_si(""))
  ) +
  scale_y_discrete(
    expand = expansion(add=c(0,0.5))
  ) + 
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.major.x = element_line(color='#A8BAC4', linewidth = 0.3),
    axis.ticks.length = unit(0, 'mm'),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color='black'),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'Econ Sans Cnd', size = 10)
  ) +
  geom_shadowtext(
    data = subset(data.by.term, n <= 1000),
    aes(n+20, y = term, label = term),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLUE,
    bg.colour = 'white',
    bg.r = 0.2,
    family = 'Econ Sans Cnd',
    size = 5
  ) +
  geom_text(
    data = subset(data.by.term, n > 1000),
    aes(20,y=term,label=term),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = 'Econ Sans Cnd',
    size = 5
  ) + 
  labs(
    title="Dataset by interaction term",
    subtitle = 'Total number of datasets by interaction term'
  ) +
  theme(
    plot.title = element_text(
      family = 'Econ Sans Cnd',
      face = 'bold',
      size = 16
    ),
    plot.subtitle = element_text(
      family = 'Econ Sans Cnd',
      size=14
    )
  ) + 
  theme(plot.margin = margin(0.01, 0.05, 0.02, 0.01, "npc"))
plt
ggsave(filename = 'images/datasets-by-term.png', plt, dpi = 300)
  
# Remove dryad datasets from DataONE and Zenodo
data.filtered <- data.all %>%
  filter((!stringr::str_detect(id, "dryad") & (source %in% c("zenodo","dataone"))) | source %in% c('dryad', 'figshare'))

data.by.source <- data.filtered %>%
  #distinct(title,pubYear, .keep_all = T) %>%
  dplyr::count(source, name='cnt') %>%
  arrange(source) %>%
  mutate(perc = cnt/sum(cnt)) %>%
  mutate(csum = rev(cumsum(rev(perc))), 
         ypos = perc/2 + lead(csum,1)) %>%
  mutate(ypos = if_else(is.na(ypos), perc/2,ypos)) %>%
  mutate(with_dryad="No Dryad duplicates")

# Number of datasets by source 
data.by.source <- data.all %>%
  #distinct(title,pubYear, .keep_all = T) %>%
  dplyr::count(source, name='cnt') %>%
  arrange(source) %>%
  mutate(perc = cnt/sum(cnt)) %>%
  mutate(csum = rev(cumsum(rev(perc))), 
         ypos = perc/2 + lead(csum,1)) %>%
  mutate(ypos = if_else(is.na(ypos), perc/2,ypos)) %>%
  mutate(with_dryad="All datasets") %>%
  bind_rows(data.by.source)

plt <- data.by.source %>%
  ggplot(aes(x="", y=perc, fill=(source))) +
  geom_col(width = 1,color=1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ with_dryad) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(aes(y = ypos,
                       label = glue::glue("{scales::percent(perc)}"), 
                       fill = source),
                   size = 4,
                   nudge_x = 0.7,
                   show.legend =F) +
  labs(title = "Percentage of datasets by source") +
  guides(fill = guide_legend(title = "Source")) +
  theme_ipsum(
    base_family = 'Arial'
  ) +
  theme(
    plot.title = element_text(size=18, face='bold', hjust = 0.5),
    axis.text.x=element_blank(), #remove x axis labels
    axis.ticks.x=element_blank(), #remove x axis ticks
    axis.text.y=element_blank(),  #remove y axis labels
    axis.ticks.y=element_blank(),  #remove y axis ticks
    axis.title = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom",
  )
plt
ggsave('images/datasets-by-source.png', plot = plt, dpi = 300)


# Top citations/downloaded datasets
data.uniq %>%
  arrange(desc(citations)) %>%
  select(id,alternateId,title,citations,downloads, views)

# Citations, downloads and views
# Remove datasets with publications DOI instead of dataset DOI (e.g. PLOSOne DOI)
journal.suffix <- data.uniq %>%
  mutate(id=stringr::str_remove(id, "doi:")) %>%
  filter(stringr::str_detect(id, "\\.s\\d+$") & !stringr::str_detect(id,"dryad")) %>%
  mutate(suffix=stringr::str_replace_all(id, "^.*/", "")) %>%
  mutate(suffix=stringr::str_split_i(suffix, "\\.", 1)) %>%
  distinct(suffix)
journal.suffix$suffix

dim(data.uniq)
metrics.clean <- data.uniq %>%
  mutate(id=stringr::str_remove(id, "doi:")) %>%
  #filter(!grepl(paste(journal.suffix$suffix, collapse = '|'), id)) %>%
  select(id, source, citations, downloads, views) 
dim(metrics.clean)

metrics.clean %>%
  summarise(tc=sum(citations), tcm=mean(citations), sdcm= sd(citations) )

metrics

metrics.clean %>%
  filter(source=='figshare') %>%
  arrange(desc(citations))

metrics.all <- metrics.clean %>%
  pivot_longer(c('downloads', 'citations', 'views'), names_to='metric', values_to = 'value')

# Metrics by source
metrics.all %>%
  group_by(source, metric) %>%
  dplyr::summarise(value=sum(value))

data.uniq  %>%
  group_by(source) %>%
  dplyr::summarise(citations=sum(citations))

metrics <- metrics.all %>% 
  group_by(metric) %>%
  dplyr::summarise(value=sum(value))
metrics

plt <- metrics %>%
  ggplot() + 
  geom_col(aes(value,metric), fill=BLUE, width=0.8) +
  scale_x_continuous(
    #limits=c(0, 86700), #TODO: fix limits
    #breaks = seq(0,max(metrics$value),1000), #TODO: fix breaks
    expand = c(0, 0),
    position = 'top',
    labels = scales::label_number_si()
  ) +
  scale_y_discrete(
    expand = expansion(add=c(0,0.5))
  ) + 
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.major.x = element_line(color='#A8BAC4', linewidth = 0.3),
    axis.ticks.length = unit(0, 'mm'),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color='black'),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'Econ Sans Cnd', size = 10)
  ) +
  geom_shadowtext(
    data =  subset(metrics, value < 3550),
    aes(value+20, y = metric, label = metric),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLUE,
    bg.colour = 'white',
    bg.r = 0.2,
    family = 'Econ Sans Cnd',
    size = 5
  ) + geom_text(
    data = subset(metrics, value >= 3550),
    aes(30,y=metric,label=metric),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = 'Econ Sans Cnd',
    size = 5
  ) + 
  labs(
    title="Dataset Metrics",
    subtitle = 'Total number of downloads, views and citations'
  ) +
  theme(
    plot.title = element_text(
      family = 'Econ Sans Cnd',
      face = 'bold',
      size = 16
    ),
    plot.subtitle = element_text(
      family = 'Econ Sans Cnd',
      size=14
    )
  ) + 
  theme(plot.margin = margin(0.01, 0.05, 0.02, 0.01, "npc"))
plt

metrics.all %>%
  filter(value > 0) %>%
  ggplot(aes(x=metric,y=value,fill=metric)) +
  geom_boxplot(outlier.shape = NA) +
  facet_grid(metric ~ ., scales = 'free_y') +
  viridis::scale_fill_viridis(
    discrete = T,
    alpha=0.8,
    option='C'
  ) +
  scale_y_continuous(
    labels = scales::label_number_auto()
  ) +
  theme_ipsum(base_family = "Arial") +
  geom_jitter(color='black', size=0.4, alpha=0.2, position = position_jitter()) +
  theme(
    legend.position = 'none',
    plot.title = element_text(size=11)
  ) +
  ggtitle("Distribution of dataset metrics", subtitle = "Number of downloads, views and citations") +
  xlab("") +
  ylab("Total")

# Correlation between downloads and citations
metrics.cor <- metrics.clean %>%
  filter(citations > 0  & downloads > 0)
cor.test(x=metrics.cor$downloads,y=metrics.cor$citations, method = 'pearson')

metrics.clean %>%
  mutate(citations=normalize(citations), downloads=normalize(downloads), views=normalize(views)) %>%
  filter(citations > 0  & downloads > 0) %>%
  filter(citations < 1 & downloads < 1) %>%
  ggplot(aes(x=citations, y=downloads)) +
  geom_point() +
  geom_smooth(method = "lm", color="red", fill=GREEN2, se = T) +
  #scale_y_continuous(
  #  trans = 'log10'
  #) +
  theme_ipsum(
    base_family = 'Arial'
  )

# Data types (open formats x proprietary)
datatypes <- data.uniq%>%
  select(source, datatypes) %>%
  mutate(datatypes=stringr::str_split(datatypes, ";")) %>%
  unnest(datatypes)

datatypes <- datatypes %>%
  mutate(datatypes=stringr::str_split(datatypes, ";")) %>%
  unnest(datatypes)

datatypes <- datatypes %>%
  mutate(datatypes=stringr::str_replace_all(datatypes, fixed('c("'), '')) %>%
  mutate(datatypes=stringr::str_replace_all(datatypes, fixed('"'), '')) %>%
  mutate(datatypes=stringr::str_replace_all(datatypes, fixed(')'), '')) %>%
  mutate(datatypes=stringr::str_split(datatypes, ',')) %>%
  unnest(datatypes) %>%
  mutate(datatypes=stringr::str_trim(datatypes))

datatypes$types <- stringr::str_split(datatypes$datatypes, pattern = '/', simplify = T)[,1]
datatypes$subtypes <- stringr::str_split(datatypes$datatypes, pattern = '/', simplify = T)[,2]

datatypes %>%
  filter(!is.na(datatypes) & datatypes!="application/octet-stream") %>%
  group_by(datatypes) %>%
  dplyr::count(datatypes) %>%
  ungroup() %>%
  arrange(desc(n)) 


subtypes <- datatypes %>%
  filter(!is.na(datatypes) & datatypes!="application/octet-stream") %>%
  group_by(subtypes) %>%
  dplyr::count(subtypes) %>%
  ungroup() %>%
  arrange(desc(n))
subtypes

subtypes[subtypes$subtypes=='vnd.ms-excel',]$subtypes <- 'Microsoft Excel'
subtypes[subtypes$subtypes=='tab-separated-values',]$subtypes <- 'tsv'
subtypes[subtypes$subtypes=='vnd.openxmlformats-officedocument.wordprocessingml.document',]$subtypes <- 'Microsoft Word (OpenXML)'
subtypes[subtypes$subtypes=='vnd.openxmlformats-officedocument.spreadsheetml.sheet',]$subtypes <- 'Microsoft Excel (OpenXML)'

subtypes <- subtypes %>%
  dplyr::add_count(wt = n, name = 'total') %>%
  mutate(perc=round(n/total*100,2)) %>%
  select(-total)

subtypes %>% summarise(sum(perc))

subtypes <- subtypes %>%
  mutate(subtypes=factor(subtypes,subtypes)) %>%
  top_n(10)

p <- subtypes %>%
  ggplot(aes(x=subtypes,y=n)) +
  geom_segment(
    aes(x=subtypes,xend=subtypes,y=0,yend=n),
    color=ifelse(!subtypes$subtypes %in% c("Microsoft Excel"), "orange", "grey"), 
    size=ifelse(!subtypes$subtypes %in% c("Microsoft Excel"), 1.3, 0.7)
  ) + 
  geom_point(
    color=ifelse(!subtypes$subtypes %in% c("Microsoft Excel"), "orange", "grey"), 
    size=ifelse(!subtypes$subtypes %in% c("Microsoft Excel"), 5, 2)
  ) +
  ggtitle("Ten most common file formats in datasets") +
  theme_ipsum(
    base_family = 'Arial'
  ) +
  coord_flip() +
  theme(
    legend.position="none",
    plot.title = element_text(hjust = 0.9)
  ) +
  xlab("File Format") +
  ylab("Number of files")
p
ggsave('images/top10-file-formats.png', p, dpi = 300)



# Licenses

data.all$license
licenses <- data.uniq

licenses[is.na(licenses$license),]$license <- 'None'
licenses[licenses$license == "https://creativecommons.org/publicdomain/zero/1.0/",]$license <- 'CC0-1.0'
licenses[licenses$license == "https://creativecommons.org/licenses/by/4.0/",]$license <- 'CC-BY-4.0'
licenses[licenses$license == "https://creativecommons.org/licenses/by-nc/4.0/",]$license <- 'CC-BY-NC-4.0'
licenses[licenses$license == "https://opensource.org/licenses/MIT",]$license <- 'MIT'
licenses[licenses$license == "https://www.gnu.org/licenses/gpl-3.0.html",]$license <- 'GPL-3.0'
licenses[licenses$license == "https://creativecommons.org/licenses/by-nc-nd/4.0/",]$license <- 'CC-BY-ND-4.0'
licenses[licenses$license == "http://rightsstatements.org/vocab/InC/1.0/",]$license <- 'In Copyright'
licenses[licenses$license == "https://opendatacommons.org/licenses/by/summary/index.html",]$license <- 'ODC-BY 1.0'


licenses.count <- licenses %>%
  dplyr::mutate(license2=ifelse(license %in% c('CC0-1.0', 'None', 'CC-BY-4.0', 'In Copyright'), license, 'Other-Open')) %>%
  dplyr::count(license2) %>%
  arrange(n) %>%
  mutate(license2=factor(license2,license2))

licenses %>%
  select(source,license) %>%
  dplyr::count(source,license)

data.all %>%
  filter(stringr::str_detect(license,"rightsstatements")) %>%
  select(id,metadataStd,license,datatypes)

licenses.count
p <- licenses.count %>%
  ggplot(aes(x=license2,y=n)) +
  geom_segment(
    aes(x=license2,xend=license2,y=0,yend=n),
    color=ifelse(licenses.count$license2 %in% c('CC0-1.0', 'CC-BY-4.0'), "orange", "grey"), 
    size=ifelse(licenses.count$license2 %in% c('CC0-1.0', 'CC-BY-4.0'), 1.3, 1.0)
  ) + 
  geom_point(
    color=ifelse(licenses.count$license2 %in% c('CC0-1.0', 'CC-BY-4.0'), "orange", "grey"), 
    size=ifelse(licenses.count$license2 %in% c('CC0-1.0', 'CC-BY-4.0'), 5, 2)
  ) +
  theme_ipsum(
    base_family = 'Arial'
  ) +
  coord_flip() +
  theme(
    legend.position="none"
  ) +
  xlab("License") +
  ylab("Number of datasets") +
  ggtitle("Dataset Licenses") +
  ggplot2::annotate("text", x=grep("Other-Open", licenses.count$license2), y=licenses.count$n[which(licenses.count=="Other-Open")]*2.5,
           label=paste(licenses.count$n[which(licenses.count=="Other-Open")], 'datasets'),
           color="grey", size=6,angle=0,fontface="bold", hjust=0) +
  ggplot2::annotate("text", x=grep("In Copyright", licenses.count$license2), y=licenses.count$n[which(licenses.count=="In Copyright")]+100,
           label=paste(licenses.count$n[which(licenses.count=="In Copyright")], 'dataset!'),
           color="grey", size=6,angle=0,fontface="bold", hjust=0)
p
ggsave('images/licenses-types.png', p, dpi = 300)

library(forcats)
# Datasets term vs. source
data.all  %>%
  dplyr::count(source,term) %>%
  group_by(term) %>%
  dplyr::mutate(total=sum(n)) %>%
  ungroup() %>%
  arrange(total)%>%
  mutate(term=fct_reorder(term,total)) %>%
  ggplot(aes(fill=source, y=n,x=term)) +
  geom_bar(position = "stack", stat="identity") +
  scale_fill_viridis(discrete = T) +
  ggtitle("Dataset sources by interaction terms") +
  theme_ipsum(
    base_family = 'Arial'
  ) + 
  xlab("") +
  ylab("Number of datasets")

#Keywords
keywords <- data.uniq %>% 
  mutate(keywords_set=stringr::str_split(keywords, "\\s*[;,]\\s*")) %>%
  unnest(keywords_set) %>%
  select(keywords_set)

interaction_kw <- keywords %>%
  filter(keywords_set != "") %>%
  mutate(keywords_set=str_to_lower(keywords_set)) %>%
  mutate(keywords_set=str_replace_all(keywords_set, "interactions", "interaction")) %>%
  mutate(keywords_set=str_replace_all(keywords_set, "networks", "network")) %>%
  mutate(keywords_set=str_replace_all(keywords_set, "[-‒‐––]", " ")) %>%
  dplyr::count(keywords_set) %>%
  arrange(desc(n)) %>%
  ungroup() %>%
  filter(stringr::str_detect(keywords_set, "interaction")) 

interaction_kw_top20 <- interaction_kw %>% slice_max(n=20, order_by = n)
plt <- interaction_kw_top20 %>%
  mutate(keywords_set=fct_reorder(keywords_set, n)) %>%
  ggplot() + 
  geom_col(aes(n,keywords_set), fill=BLUE, width=0.8) +
  scale_x_continuous(
    #limits=c(0, 86700), #TODO: fix limits
    #breaks = seq(0,max(metrics$value),1000), #TODO: fix breaks
    expand = c(0, 0),
    position = 'top',
    labels = scales::label_number_si()
  ) +
  scale_y_discrete(
    expand = expansion(add=c(0,0.5))
  ) + 
  theme(
    panel.background = element_rect(fill="white"),
    panel.grid.major.x = element_line(color='#A8BAC4', linewidth = 0.3),
    axis.ticks.length = unit(0, 'mm'),
    axis.title = element_blank(),
    axis.line.y.left = element_line(color='black'),
    axis.text.y = element_blank(),
    axis.text.x = element_text(family = 'Econ Sans Cnd', size = 10)
  ) +
  geom_shadowtext(
    data =  subset(interaction_kw_top20, n < 200),
    aes(n+20, y = keywords_set, label = keywords_set),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLUE,
    bg.colour = 'white',
    bg.r = 0.2,
    family = 'Econ Sans Cnd',
    size = 3
  ) + geom_text(
    data = subset(interaction_kw_top20, n >= 200),
    aes(30,y=keywords_set,label=keywords_set),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = 'Econ Sans Cnd',
    size = 3
  ) + 
  labs(
    title="Most common dataset keywords",
    subtitle = 'Top 20 most frequent keywords'
  ) +
  theme(
    plot.title = element_text(
      family = 'Econ Sans Cnd',
      face = 'bold',
      size = 16
    ),
    plot.subtitle = element_text(
      family = 'Econ Sans Cnd',
      size=14
    )
  ) + 
  theme(plot.margin = margin(0.01, 0.05, 0.02, 0.01, "npc"))
plt
ggsave('images/datasets-keywords.png', plt, dpi = 150)


interaction_kw %>% filter(stringr::str_detect(keywords_set, "biological"))


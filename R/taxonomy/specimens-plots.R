library(dplyr)

interactions <- read.csv('data/interactions-taxon-latlon-time.tsv.gz', sep = '\t', quote="")
head(interactions)

# Replace empty for NA
interactions <- interactions %>%
  dplyr::mutate(
    decimalLatitude=replace(decimalLatitude, decimalLatitude=="", NA), 
    decimalLongitude=replace(decimalLongitude, decimalLongitude=="", NA),
    eventDate=replace(eventDate, eventDate=="", NA)
    )

counts.by.rank <- interactions %>%
  filter(stringr::str_starts(sourceTaxonId, "GBIF:") & stringr::str_starts(targetTaxonId, "GBIF:")) %>%
  dplyr::mutate(hasloc=!is.na(decimalLatitude) & !is.na(decimalLongitude), hasdate=!is.na(eventDate), haslocdate=!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(eventDate)) %>%
  #dplyr::select(sourceTaxonKingdomName,sourceTaxonSpeciesName, sourceTaxonRank, hasloc,hasdate,haslocdate) %>%
  #dplyr::rename(kingdomName=sourceTaxonKingdomName, speciesName=sourceTaxonSpeciesName,rank=sourceTaxonRank) %>%
  bind_rows(
    interactions %>%
    filter(stringr::str_starts(sourceTaxonId, "GBIF:") & stringr::str_starts(targetTaxonId, "GBIF:")) %>%
    dplyr::mutate(hasloc=!is.na(decimalLatitude) & !is.na(decimalLongitude), hasdate=!is.na(eventDate), haslocdate=!is.na(decimalLatitude) & !is.na(decimalLongitude) & !is.na(eventDate))
    #dplyr::select(targetTaxonKingdomName,targetTaxonSpeciesName, targetTaxonRank, hasloc,hasdate,haslocdate) %>%
    #dplyr::rename(kingdomName=targetTaxonKingdomName, speciesName=targetTaxonSpeciesName,rank=targetTaxonRank)
  )
counts.by.rank

# Counting species rank
counts.species.rank <- counts.by.rank %>%
  distinct(sourceTaxonKingdomName, sourceTaxonSpeciesName, .keep_all=T) %>%
  group_by(sourceTaxonKingdomName) %>%
  dplyr::summarise(n=n(),nloc=sum(hasloc), ndate=sum(hasdate), nlocdate=sum(haslocdate))
counts.species.rank

count.up.species.rank <- counts.by.rank %>%
  distinct(sourceTaxonKingdomName, sourceTaxonPhylumName, sourceTaxonClassName, sourceTaxonOrderName, sourceTaxonFamilyName, sourceTaxonGenusName, .keep_all = T) %>%
  group_by(sourceTaxonKingdomName) %>%
  dplyr::summarise(n=n(),nloc=sum(hasloc), ndate=sum(hasdate), nlocdate=sum(haslocdate))
count.up.species.rank


counts.by.rank <- counts.species.rank %>%
  mutate(group="species") %>%
  bind_rows(count.up.species.rank %>% mutate(group="other")) %>%
  filter(sourceTaxonKingdomName!="incertae sedis" & sourceTaxonKingdomName!="Viruses")  %>%
  mutate(sourceTaxonKingdomName=as.factor(stringr::str_trim(stringr::str_replace_all(sourceTaxonKingdomName,'"', ""))))
counts.by.rank %>%
  arrange(desc(n))

counts.by.rank <- counts.by.rank %>%
  group_by(sourceTaxonKingdomName) %>%
  dplyr::mutate(tot=sum(n)) %>%
  ungroup() %>%
  dplyr::mutate(np=n/tot*100,nlocp=nloc/tot*100,ndatep=ndate/tot*100, nlocdatep=nlocdate/tot*100) %>%
  dplyr::select(sourceTaxonKingdomName, group, nlocp,ndatep,nlocdatep) %>% 
  complete(sourceTaxonKingdomName,group, fill = list(nlocp=0,ndatep=0,nlocdate=0)) %>% 
  gather(key="measure", value="value", -c(1,2))
counts.by.rank

counts.by.rank$measure[counts.by.rank$measure == 'nlocp'] <- 'w/ location'
counts.by.rank$measure[counts.by.rank$measure == 'ndatep'] <- 'w/ time'
counts.by.rank$measure[counts.by.rank$measure == 'nlocdatep'] <- 'w/ loc. and time'

# Set a number of 'empty bar' to add at the end of each group
empty_bar <- 2
nObsType <- nlevels(as.factor(counts.by.rank$group))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(counts.by.rank$sourceTaxonKingdomName)*nObsType, ncol(counts.by.rank)) )
colnames(to_add) <- colnames(counts.by.rank)
to_add$sourceTaxonKingdomName <- rep(levels(counts.by.rank$sourceTaxonKingdomName), each=empty_bar*nObsType )
data <- rbind(counts.by.rank, to_add)
data <- data %>% arrange(sourceTaxonKingdomName, measure)
data$id <- rep( seq(1, nrow(data)/nObsType) , each=nObsType)

data
# Get the name and the y position of each label
label_data <- data %>% ungroup() %>% dplyr::group_by(id, measure) %>% dplyr::summarize(tot=sum(value))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data <- data %>% 
  group_by(sourceTaxonKingdomName) %>% 
  dplyr::summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  dplyr::mutate(title=mean(c(start, end)))

#


data
# Make the plot
p <- data %>% 
  ggplot() +      
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=group), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE, name="Rank", option = 'D', labels=c("Higher ranks/Unranked", "Species"), alpha = 1) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(data$id),6), y = c(0, 20, 40, 60, 80, 100), label = c("0%", "20%", "40%", "60%", "80%", "100%") , color="grey", size=2 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-80,max(label_data$tot, na.rm=T)+10) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0.1,4), "cm"),
    plot.title = element_text(
      family = 'Econ Sans Cnd',
      face = 'bold',
      size = 12
    ),
    plot.subtitle = element_text(
      family = 'Econ Sans Cnd',
      size=8
    )
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=measure, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=1.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = c(-20,-27,-27,-27,-20,-27,-27), label=sourceTaxonKingdomName), hjust=c(0.5,0.5,0.6,0.5,0.5,0.5,0.4), colour = viridis(n=nlevels(base_data$sourceTaxonKingdomName)), alpha=0.8, size=2, fontface="bold", inherit.aes = FALSE) +
  labs(
    title="Completeness of biotic interaction records",
    subtitle = 'Percentage of records with location, time and both location and time data'
  )
p
ggsave(p, file="images/completeness.png", dpi=300, bg="white")

library(bibliometrix)
library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(tidytext)
library(tm)
library(textstem)
library(SnowballC)
library(topicmodels)
library(parallel)
library(doParallel)
library(ldatuning)
library(hrbrthemes)

#'
#' BIBLOMETRIC AND TEXT-MINING ANALYSIS
#' @author Jose A Salim
#' 
#'
#'

# Auxiliary functions
source('functions.R')

# Pre-processing
source('preprocess_pubs.R')

# Topic Modeling
source('topic-modelling.R')

# Clustering
source('corpus-clustering.R')


research.areas.group <- df.uniq %>% 
  mutate(area=stringr::str_split(Fields.of.Research..ANZSRC.2020.,"\\s*;\\s*")) %>%
  unnest(area) %>%
  select(GROUP, area) %>%
  filter(stringr::str_detect(area,"^31[0-9]+")) %>%
  mutate(area=stringr::str_remove_all(area,'^\\s*[0-9]+\\s*')) %>%
  filter(area!='Ecology') %>%
  group_by(area, GROUP) %>%
  dplyr::summarise(n=n()) %>%
  mutate(GROUP=stringr::str_remove(GROUP,'\\s*interaction')) %>%
  arrange(GROUP, desc(n))
research.areas.group

library(ggplot2)
library(viridis)
library(hrbrthemes)

plt <- ggplot(research.areas.group, aes(fill=area, y=n, x=forcats::fct_reorder(GROUP, n))) + 
  geom_bar(position="fill", stat="identity") +
  scale_fill_viridis(
    discrete = T, 
    guide = guide_legend(nrow=4, title = 'Research area', title.position = 'top')
    #guide = guide_legend(size=1, keyheight = unit(2, units = "mm"), keywidth=unit(2, units = "mm"), label.position = "bottom", title.position = 'top', nrow=4)
  ) +
  
  ggtitle("Distribution of research areas per class of publications") +
  scale_y_continuous(
    labels = scales::percent
  ) +
  theme_ipsum_rc() +
  theme(
    plot.title = element_text(hjust = 0.8, size = 10),
    panel.spacing = unit(0,'lines'),
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  ) +
  ylab("") +
  xlab("")
plt
ggsave('images/research-areas-class.png', plt, dpi = 300)

# Plot top 20 tokens (lemma)
g1 <- tokens %>%
  count(lemma) %>%
  arrange(desc(n)) %>%
  top_n(20) %>%
  ggplot() + 
  geom_bar(aes(x=  reorder(lemma, n), y = n), stat = 'identity') +
  coord_flip() + 
  xlab("")+
  theme_bw()
g1

##################
# Compare groups #
##################


# Correlograms

dtm.cor <- df.pos.uniq %>%
  group_by(doc_id,lemma) %>%
  dplyr::add_count(lemma) %>%
  bind_tf_idf(document = doc_id, term = lemma, n=n) %>%
  ungroup() %>%
  replace_na(list(tf_idf=0)) %>%
  group_by(GROUP) %>%
  dplyr::mutate(i1 = row_number()) %>% 
  pivot_wider(id_cols = i1, names_from = c(GROUP), values_from = c(n)) %>%
  dplyr::select(-i1)
dtm.cor

ggpairs(dtm.cor, upper = list(continuous = wrap("cor", method = "spearman")))


# Most frequent words in all publications
dtm.tidy
ratios <- df.pos.uniq %>%
  group_by(GROUP, lemma) %>%
  group_by(lemma) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  dplyr::select(group, term, n) %>%
  pivot_wider(names_from = group, values_from = n, values_fill = list(n = 0)) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(across2x(contains('interaction'), contains('interaction'), ~log(.x/.y), 
                  .names = 'logratio_{xcol}_{ycol}', .comb = 'minimal',
                  .names_fn = ~ gsub(" ","", gsub("interaction","", .x)))) %>%
  dplyr::select(term, contains('logratio_'))

ratios
names(ratios)

for (name in names(ratios)) {
  if (stringr::str_starts(name, 'logratio_.*')) {
    labels = str_split(name, '_', simplify = T)[1,2:3]
    varname <- as.name(name)
    p <- ratios %>% 
      group_by(!!varname < 0) %>%
      slice_max(abs(!!varname), n = 15) %>%
      ungroup() %>%
      mutate(term = reorder(term, !!varname)) %>%
      ggplot(aes(term, !!varname, fill = !!varname < 0)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      ylab("log ratios") + 
      scale_fill_discrete(name="", labels = labels)   
    plot(p)
    readline(prompt="Press [enter] to continue")
  }
}

# Plot number of publications per year per group
# remove 2023 publications
df.uniq <- read.csv('../../python/data/dimensions/publications-ecology-filtered-13701.csv')
dim(df.uniq)
df.uniq %>%
  filter(PubYear < 2023) %>%
  group_by(GROUP) %>%
  dplyr::count(GROUP) %>%
  mutate(total=sum(n)) %>%
  arrange(desc(n))
  

agg_year <- df.uniq[df.uniq$PubYear < 2023,] %>%
  filter(!is.na(PubYear)) %>%
  dplyr::count(GROUP, PubYear)
py_plot <- ggplot(agg_year, aes(x=as.integer(PubYear), y=n, group=GROUP, color=GROUP)) +
ggtitle(expression("Number of publications including the term \"biotic interaction\"\nand other similar terms throughout the years")) +
  xlab('Year') +
  ylab('Number of publications') +
  guides(color = guide_legend(title = "Term")) +
  scale_x_continuous(
    limits = c(1970,2022)
  ) +
  geom_line() +
  theme_ipsum(
    base_family = 'Arial'
  ) + 
  theme(
    plot.title = element_text(size = 12),
    axis.text.x = element_text(angle = 45),
    legend.position = "bottom"
  )
py_plot
ggsave('images/pubs-by-year.png', py_plot, dpi = 300)


for (g in unique(df$GROUP)) {
  df.group = df[df$GROUP == g & !is.na(df$PY),]
  net <- biblioNetwork(df.group, analysis = "co-occurrences", network = "keywords", sep = ";", short = T)
  netstat <- networkStat(net, stat = 'all', type='all')
  summary(netstat,k=10)
  
  netplot=networkPlot(net, normalize="association", n = 10, Title = paste("Keyword Co-occurrences", g, sep = ' - '), type = "fruchterman", 
                      size.cex=TRUE, size=20, remove.multiple=F, edgesize = 10, labelsize=5,label.cex=TRUE,label.n=10,edges.min=2)
  Map=thematicMap(df.group, field = "DE", n = 25, minfreq = 5, stemming = T, size = 0.7, n.labels=5, repel = TRUE)
  plot(Map$map)
  readline(prompt="Press [enter] to continue")
}

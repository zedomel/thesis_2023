library(dplyr)


# Load data
ans <- read.csv('data/ppi-respostas.tsv', sep = '\t')
names(ans) <- c('date','name','institution', 'position','email','animal','plant','env','int1', 'int2','q1','q2','obs')
ans


q.labels <- c('Are the terms sufficient?', 'Are the terms\nusually sampled?')
names(q.labels) <- c('q1', 'q2')

# Plots
plt <- ans %>%
  select(q1,q2) %>%
  pivot_longer(cols = c(q1,q2)) %>%
  mutate(value=ifelse(value=='Sim', 'Yes', 'No')) %>%
  mutate(value=as.factor(value)) %>%
  dplyr::group_by(name,value) %>%
  dplyr::summarise(perc=n()/29) %>%
  mutate(labels = scales::percent(perc)) %>%
  ungroup() %>%
  dplyr::group_by(name) %>%
  ggplot(aes(x="", y=perc, fill=(value))) +
  geom_col(width = 1,color=1) +
  coord_polar("y", start = 0) +
  facet_wrap(~ name, labeller = labeller(name=q.labels)) +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  labs(title = "Survey responses") +
  guides(fill = guide_legend(title = "Answer")) +
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
ggsave('images/survey_questions.png', plt, dpi = 300)


animal.en <- c('Biological elements', 'Behavior', 'Activity season', 'Body size', 'Feeding habitat', 'Taxonomic elements', 
               'Specimen', 'Nesting habitat', 'Sociality', 'Nesting substrate', 'Caste', 'References')
animals <- ans %>%
  mutate(animals=stringr::str_split(animal, '(?![^\\(]*\\))\\s*,\\s*')) %>%
  unnest(animals) %>%
  select(animals) %>%
  filter(animals!="") %>%
  dplyr::count(animals) %>%
  arrange(desc(n)) %>%
  filter(n>1) %>%
  mutate(animals=factor(animals, animals, labels=animal.en), 
         perc=n/29)
animals

update_geom_font_defaults()
extrafont::loadfonts()

library(forcats)
plt <- animals %>%
  mutate(animals = fct_reorder(animals, perc)) %>%
  ggplot( aes(x=animals, y=perc)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  labs(title = "Most relevent terms for Animals") +
  xlab("") +
  ylab("Percentage of anwsers") +
  theme_ipsum_rc(
    plot_title_margin = 10
  ) + theme(
    plot.title = element_text(hjust = 1)
  )
plt
ggsave('images/survey-animals.png',plt, dpi = 300)


plants.en <- c('Flower longevity', 'Bloom intensity', 'Flower type', 'Flower color', 'Breeding system', 'Taxonomic elements', 'Floral odor', 
               'Establishment means', 'Floral nectar', 'Flower per plant', 'Life form', 'Num. observed flowers', 'Num. flowering plants', 'Invidiual count'
               )
plants <- ans %>%
  mutate(plants=stringr::str_split(plant, '(?![^\\(]*\\))\\s*,\\s*')) %>%
  unnest(plants) %>%
  select(plants) %>%
  filter(plants!="") %>%
  dplyr::count(plants) %>%
  arrange(desc(n)) %>%
  filter(n>=20) %>%
  mutate(plants=factor(plants, plants, labels=plants.en), 
         perc=n/29)
plants

plt <- plants %>%
  mutate(plants = fct_reorder(plants, perc)) %>%
  ggplot( aes(x=plants, y=perc)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  labs(title = "Most relevent terms for Plants") +
  xlab("") +
  ylab("Percentage of anwsers") +
  theme_ipsum_rc(
    plot_title_margin = 10
  ) + theme(
    plot.title = element_text(hjust = 1)
  )
plt
ggsave('images/survey-plants.png',plt, dpi = 300)


int.en <- c('Collected resources', 'Interaction type', 'Visitor behavior', 'Place of contact', 'Time observing the plant', 
           'Sampling effort', 'Sampling method', 'Fruit set', 'Num. of visited flowers', 'Seed set', 'Cardinality (++,+-,--)',
           'Frequency', 'Total geographic area')
interact <- ans %>%
  mutate(int=stringr::str_c(int1,int2, sep = ',')) %>%
  mutate(int=stringr::str_split(int, '(?![^\\(]*\\))\\s*,\\s*')) %>%
  unnest(int) %>%
  select(int) %>%
  filter(int!="" & int!="Esforço amostral (Sampling effort)") %>%
  dplyr::count(int) %>%
  arrange(desc(n)) %>%
  filter(n>=20) %>%
  mutate(int=factor(int, int, labels=int.en), 
         perc=n/29)
interact

plt <- interact %>%
  mutate(int = fct_reorder(int, perc)) %>%
  ggplot( aes(x=int, y=perc)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  labs(title = "Most relevent terms for Interactions") +
  xlab("") +
  ylab("Percentage of anwsers") +
  theme_ipsum_rc(
    plot_title_margin = 10
  ) + theme(
    plot.title = element_text(hjust = 0.9)
  )
plt
ggsave('images/survey-int.png',plt, dpi = 300)

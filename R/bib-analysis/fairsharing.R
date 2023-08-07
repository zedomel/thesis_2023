library(dplyr)
library(stringr)
library(tidyr)
library(grid)
library(tidyverse)
library(shadowtext)

source('colors.R')

data <- read.csv('./standards.tsv', sep = '\t', header = F)
names(data) <- c('id', 'name', 'year', 'description', 'domains', 'subjects', 'main_subject' )
subjects <- data %>%
  mutate(s = purrr::map(strsplit(main_subject, ';'), 1)) %>%
  unnest(s) %>%
  group_by(s) %>%
  filter(!is.na(s) & s != 'Biology') %>%
  count(s) %>%
  arrange(n)

max(subjects$n)


plt <- ggplot(subjects) + 
  geom_col(aes(n,s), fill=BLUE, width=0.8) +
  scale_x_continuous(
    limits=c(0, 280),
    breaks = seq(0,280,20),
    expand = c(0, 0),
    position = 'top'
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
    data = subset(subjects, n < 50),
    aes(n, y = s, label = s),
    hjust = 0,
    nudge_x = 0.3,
    colour = BLUE,
    bg.colour = 'white',
    bg.r = 0.2,
    family = 'Econ Sans Cnd',
    size = 3
  ) + geom_text(
    data = subset(subjects, n >= 50),
    aes(0,y=s,label=s),
    hjust = 0,
    nudge_x = 0.3,
    colour = "white",
    family = 'Econ Sans Cnd',
    size = 3
  ) + 
  labs(
    title="FAIRsharing standards by subject",
    subtitle = 'Number of standards in Biology field ready for use in FAIRsharing'
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
ggsave('fairsharing.png', plot = plt, dpi=300)


subjects %>%
  ungroup() %>%
  filter(s %in% c('Ecology', 'Zoology', 'Botany', 'Environmental Science')) %>%
  summarise(sum = sum(n))

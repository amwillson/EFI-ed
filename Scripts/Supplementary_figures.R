library(tidyverse)
library(pivottabler)
library(RColorBrewer)
library(cowplot)
library(forcats)

rm(list = ls())

load('Data/cleaned_data_EF_course.RData')
EF_data = data
rm(data)

EF_data %>%
  filter(Education.Level %in% c('Undergraduate', 'Undergraduate & Graduate')) %>%
  group_by(Sub.topic, Carnegie.Classification.2) %>%
  dplyr::count() %>%
  mutate(Sub.topic = factor(Sub.topic, 
                            levels = c('Basics of Forecasting',
                                       'Workflows & Open Science',
                                       'Probability & Uncertainty',
                                       'Statistical Models',
                                       'Basics of Ecology',
                                       'Data Assimilation',
                                       'State Space Models',
                                       'Decision Science',
                                       'Working with Data',
                                       'Basics of Coding',
                                       'Basics of Statistics',
                                       'Model Assessment',
                                       'Mechanistic Models',
                                       'Ethics',
                                       'Machine Learning',
                                       'Data Manipulation',
                                       'Data Sources'))) %>%
  ggplot(aes(x = fct_rev(Sub.topic), y = n, fill = Carnegie.Classification.2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_linedraw() +
  xlab('') + ylab('Number of Lessons') +
  labs(fill = 'Carnegie') +
  ggtitle('Undergraduate Forecasting Courses') +
  theme(plot.title = element_text(size = 14, hjust = 0.4),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_manual(values = c('B' = '#c7e9b4', 'R1' = '#225ea8'))

ggsave(last_plot(), filename = 'Plots/undergraduate_barplot_ef_carnegie.jpeg')

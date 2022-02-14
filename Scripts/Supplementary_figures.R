library(tidyverse)
library(pivottabler)
library(RColorBrewer)
library(cowplot)
library(forcats)

rm(list = ls())

# Load data
load('Data/cleaned_data_EF_course.RData')
EF_data = data
load('Data/cleaned_data.RData')
course_data = data
rm(data)

## Forecasting Course by Institution Type ##

B_count = 1 # number of baccalaureate schools
R1_count = 4 # number of R1 schools

EF_data = EF_data %>%
  filter(Carnegie.Classification.2 != 'NA') %>%
  mutate(Carnegie.Classification.2 = as.factor(Carnegie.Classification.2))

EF_data_count = EF_data %>%
  group_by(Sub.topic, Carnegie.Classification.2) %>%
  dplyr::count()

EF_data_count = EF_data_count %>%
  mutate(n_stan = if_else(Carnegie.Classification.2 == 'B', n / B_count, n / R1_count))

EF_data_count$Sub.topic = factor(EF_data_count$Sub.topic,
                                 levels = c('Basics of Forecasting',
                                            'Workflows & Open Science',
                                            'Statistical Models',
                                            'Basics of Ecology',
                                            'Working with Data',
                                            'Basics of Statistics',
                                            'Probability & Uncertainty',
                                            'Model Assessment',
                                            'Decision Science',
                                            'Data Assimilation',
                                            'Mechanistic Models',
                                            'Basics of Coding',
                                            'Data Sources',
                                            'State Space Models',
                                            'Ethics',
                                            'Data Manipulation'))

p1 = EF_data_count %>%  
  ggplot(aes(x = fct_rev(Sub.topic), y = n_stan, fill = Carnegie.Classification.2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_linedraw() +
  labs(fill = 'Carnegie') +
  ggtitle('Forecasting Courses') +
  xlab('') + ylab('Lessons Per Institution') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_manual(values = c('B' = '#c7e9b4', 'R1' = '#225ea8'))

#EF_data_count$Sub.topic = factor(EF_data_count$Sub.topic,
#                                 levels = c('Basics of Forecasting',
#                                            'Workflows & Open Science',
#                                            'Statistical Models',
#                                            'Basics of Ecology',
#                                            'Working with Data',
#                                            'Basics of Statistics',
#                                            'Probability & Uncertainty',
#                                            'Model Assessment',
#                                            'Decision Science',
#                                            'Data Assimilation',
#                                            'Basics of Coding',
#                                            'State Space Models',
#                                            'Mechanistic Models',
#                                            'Ethics',
#                                            'Data Sources',
#                                            'Data Manipulation'))

#EF_data_count %>%  
#  ggplot(aes(x = fct_rev(Sub.topic), y = n, fill = Carnegie.Classification.2)) +
#  geom_bar(stat = 'identity') +
#  coord_flip() +
#  theme_linedraw() +
#  labs(fill = 'Carnegie') +
#  ggtitle('Forecasting Courses') +
#  xlab('') + ylab('Number of Courses') +
#  theme(plot.title = element_text(size = 14, hjust = 0.5),
#        legend.title = element_text(size = 12, hjust = 0.5),
#        legend.text = element_text(size = 10),
#        axis.title = element_text(size = 12),
#        axis.text = element_text(size = 10)) +
#  scale_fill_manual(values = c('R1' = '#225ea8', 'B' = '#c7e9b4'))

## Forecasting-Adjacent Course by Institution Type ##

course_count = course_data %>%
  distinct(College, Carnegie.classification.2) %>%
  group_by(Carnegie.classification.2) %>%
  dplyr::count()

course_data_count = course_data %>%
  group_by(Sub.topic, Carnegie.classification.2) %>%
  dplyr::count()

course_data_count = course_data_count %>%
  mutate(n_stan = case_when(Carnegie.classification.2 == 'A' ~ n / 18,
                            Carnegie.classification.2 == 'A/B' ~ n / 2,
                            Carnegie.classification.2 == 'B' ~ n / 6,
                            Carnegie.classification.2 == 'D/PU' ~ n / 5,
                            Carnegie.classification.2 == 'M1' ~ n / 5,
                            Carnegie.classification.2 == 'M3' ~ n / 3,
                            Carnegie.classification.2 == 'R1' ~ n / 2,
                            Carnegie.classification.2 == 'R2' ~ n / 5,
                            Carnegie.classification.2 == 'TC' ~ n / 2))

course_data_count$Sub.topic = factor(course_data_count$Sub.topic,
                                     levels = c('Basics of Ecology',
                                                'Basics of Statistics',
                                                'Basics of Coding',
                                                'Statistical Models',
                                                'Data Sources',
                                                'Working with Data',
                                                'Science Communication',
                                                'Data Manipulation',
                                                'Ethics',
                                                'Mechanistic Models',
                                                'Basics of Forecasting',
                                                'Machine Learning',
                                                'Probability & Uncertainty',
                                                'Data Visualization',
                                                'Decision Science',
                                                'Workflows & Open Science',
                                                'Traditional Ecological Knowledge',
                                                'Model Assessment'))
p2 = course_data_count %>%
  ggplot(aes(x = fct_rev(Sub.topic), y = n_stan, fill = Carnegie.classification.2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_linedraw() +
  xlab('') + ylab('Courses per Institution') +
  ggtitle('Forecasting-Adjacent Courses') +
  labs(fill = 'Carnegie') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10)) +
  scale_fill_manual(values = c('#ffffd9',
                               '#edf8b1',
                               '#c7e9b4',
                               '#7fcdbb',
                               '#41b6c4',
                               '#1d91c0',
                               '#225ea8',
                               '#253494',
                               '#081d58'))

pg = plot_grid(p1 + theme(legend.position = 'none'), p2, 
               nrow = 1, rel_widths = c(0.5, 0.65),
               labels = c('A', 'B'))
pg
ggsave(pg, filename = 'Plots/barplot_carnegie_subtopic.jpeg', 
       width = 12, height = 6, units = 'in')

## This script contains the final plots used to visualize the data
## at each curriculum level
## The figure numbers correspond to the figure numbers of the manuscript
## Supplementary figures are compiled in a separate document

## Authors: AM Willson & H Gallo
## Date modified: 17 March 2022

library(tidyverse)
library(pivottabler)
library(RColorBrewer)
library(cowplot)
library(readxl)
library(usmap)

#### Figure 2 ####

## Online resources plot ##

levels_disc = c('Basics of Coding',
                'Basics of Ecology',
                'Basics of Forecasting',
                'Basics of Statistics',
                'Data Sources',
                'Data Assimilation',
                'Machine Learning',
                'Mechanistic Models',
                'Model Assessment',
                'Probability & Uncertainty',
                'State Space Models',
                'Statistical Models',
                'Data Manipulation',
                'Data Visualization',
                'Workflows & Open Science',
                'Working with Data',
                'Science Communication',
                'Social Science',
                'Ethics',
                'Traditional Ecological Knowledge')

# Load online resources data
load('Data/cleaned_data_online.RData')
# Rename to avoid confusion
online_data = data
rm(data)

# Find total number of instances each topic was covered across courses
EF_online_resources_df = online_data %>%
  group_by(Category) %>%
  dplyr::summarize(n())
colnames(EF_online_resources_df) = c('Topic', 'Number of Resources')
EF_online_resources_df$Topic = as.character(EF_online_resources_df$Topic)

# Make ordered factor so pie slices are in order
EF_online_resources_df$Topic = factor(EF_online_resources_df$Topic,
                                      levels = levels_disc)

pie_chart_online_resources <- ggplot(EF_online_resources_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  labs(title = "Open-access, Online Resources", subtitle = 'n = 409') +
  theme_void() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = 'none')

p1 = pie_chart_online_resources +
  scale_fill_manual(values = c('Basics of Coding' = '#2C0845',
                               'Basics of Ecology' = '#56396A',
                               'Basics of Forecasting' = '#806B8F',
                               'Basics of Statistics' = '#AB9CB5',
                               'Data Sources' = '#D5CEDA',
                               'Data Assimilation' = '#1C3706',
                               'Machine Learning' = '#3C542A',
                               'Mechanistic Models' = '#5D704D',
                               'Model Assessment' = '#7D8D71',
                               'Probability & Uncertainty' = '#9EA994',
                               'State Space Models' = '#BEC6B8',
                               'Statistical Models' = '#DFE2DB',
                               'Data Manipulation' = '#65061D',
                               'Data Visualization' = '#8C4456',
                               'Workflows & Open Science' = '#B2838E',
                               'Working with Data' = '#D9C1C7',
                               'Science Communication' = '#043255',
                               'Social Science' = '#8299AA',
                               'Ethics' = '#462A09',
                               'Traditional Ecological Knowledge' = '#A39584'))

## Forecasting Courses Plot ##

# Load forecasting course data
load('Data/cleaned_data_EF_course.RData')
# Rename to avoid confusion
ef_course_data = data
rm(data)

# Find total number of instances each topic was covered across courses
EF_course_data_df = ef_course_data %>%
  group_by(Sub.topic) %>%
  dplyr::summarize(n())
colnames(EF_course_data_df) = c('Topic', 'Number of Resources')

EF_course_data_df$Topic = factor(EF_course_data_df$Topic,
                                 levels = levels_disc)

pie_chart_EF_courses <- ggplot(EF_course_data_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  labs(title = "Forecasting Course Lessons",
       subtitle = 'n = 192') +
  theme_void() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = 'none')

p2 = pie_chart_EF_courses +
  scale_fill_manual(values = c('Basics of Coding' = '#2C0845',
                               'Basics of Ecology' = '#56396A',
                               'Basics of Forecasting' = '#806B8F',
                               'Basics of Statistics' = '#AB9CB5',
                               'Data Sources' = '#D5CEDA',
                               'Data Assimilation' = '#1C3706',
                               'Machine Learning' = '#3C542A',
                               'Mechanistic Models' = '#5D704D',
                               'Model Assessment' = '#7D8D71',
                               'Probability & Uncertainty' = '#9EA994',
                               'State Space Models' = '#BEC6B8',
                               'Statistical Models' = '#DFE2DB',
                               'Data Manipulation' = '#65061D',
                               'Data Visualization' = '#8C4456',
                               'Workflows & Open Science' = '#B2838E',
                               'Working with Data' = '#D9C1C7',
                               'Science Communication' = '#043255',
                               'Social Science' = '#8299AA',
                               'Ethics' = '#462A09',
                               'Traditional Ecological Knowledge' = '#A39584'))

## Forecasting-Adjacent Courses Plot ##

# Load forecasting-adjacent course data
load('Data/cleaned_data.RData')
# Rename to avoid confusion
course_data = data
rm(data)

# Find total number of instances each topic was covered across courses
FA_course_data_df = course_data %>%
  group_by(Sub.topic) %>%
  dplyr::summarize(n())
colnames(FA_course_data_df) = c('Topic', 'Number of Resources')

# Make ordered factor so pie slieces are in order
FA_course_data_df$Topic = factor(FA_course_data_df$Topic,
                                 levels = levels_disc)

pie_chart_FA_courses <- ggplot(FA_course_data_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  labs(title = "Forecasting-Adjacent Courses",
       subtitle = 'n = 1,485') +
  theme_void() +
  theme(plot.title = element_text(size = 18, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.position = 'none')

p3 = pie_chart_FA_courses +
  scale_fill_manual(values = c('Basics of Coding' = '#2C0845',
                               'Basics of Ecology' = '#56396A',
                               'Basics of Forecasting' = '#806B8F',
                               'Basics of Statistics' = '#AB9CB5',
                               'Data Sources' = '#D5CEDA',
                               'Data Assimilation' = '#1C3706',
                               'Machine Learning' = '#3C542A',
                               'Mechanistic Models' = '#5D704D',
                               'Model Assessment' = '#7D8D71',
                               'Probability & Uncertainty' = '#9EA994',
                               'State Space Models' = '#BEC6B8',
                               'Statistical Models' = '#DFE2DB',
                               'Data Manipulation' = '#65061D',
                               'Data Visualization' = '#8C4456',
                               'Workflows & Open Science' = '#B2838E',
                               'Working with Data' = '#D9C1C7',
                               'Science Communication' = '#043255',
                               'Social Science' = '#8299AA',
                               'Ethics' = '#462A09',
                               'Traditional Ecological Knowledge' = '#A39584'))

## Legends
# Separate legends by color to add manually by cowplot
leg_1 = EF_online_resources_df %>%
  filter(Topic %in% c('Basics of Coding',
                      'Basics of Ecology',
                      'Basics of Forecasting',
                      'Basics of Statistics',
                      'Data Sources')) %>%
  ggplot(aes(x = '', y = `Number of Resources`, fill = Topic)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c('Basics of Coding' = '#2C0845',
                               'Basics of Ecology' = '#56396A',
                               'Basics of Forecasting' = '#806B8F',
                               'Basics of Statistics' = '#AB9CB5',
                               'Data Sources' = '#D5CEDA'))

leg_2 = EF_online_resources_df %>%
  filter(Topic %in% c('Data Assimilation',
                      'Machine Learning',
                      'Mechanistic Models',
                      'Model Assessment',
                      'Probability & Uncertainty',
                      'State Space Models',
                      'Statistical Models')) %>%
  ggplot(aes(x = '', y = `Number of Resources`, fill = Topic)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c('Data Assimilation' = '#1C3706',
                               'Machine Learning' = '#3C542A',
                               'Mechanistic Models' = '#5D704D',
                               'Model Assessment' = '#7D8D71',
                               'Probability & Uncertainty' = '#9EA994',
                               'State Space Models' = '#BEC6B8',
                               'Statistical Models' = '#DFE2DB'))

leg_3 = EF_online_resources_df %>%
  filter(Topic %in% c('Data Manipulation',
                      'Data Visualization',
                      'Workflows & Open Science',
                      'Working with Data')) %>%
  ggplot(aes(x = '', y = `Number of Resources`, fill = Topic)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c('Data Manipulation' = '#65061D',
                               'Data Visualization' = '#8C4456',
                               'Workflows & Open Science' = '#B2838E',
                               'Working with Data' = '#D9C1C7'))

leg_4 = EF_online_resources_df %>%
  filter(Topic %in% c('Science Communication',
                      'Social Science',
                      'Ethics',
                      'Traditional Ecological Knowledge')) %>%
  ggplot(aes(x = '', y = `Number of Resources`, fill = Topic)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar('y', start = 0) +
  theme_void() +
  theme(legend.title = element_blank(),
        legend.text = element_text(size = 12)) +
  scale_fill_manual(values = c('Science Communication' = '#043255',
                               'Social Science' = '#8299AA',
                               'Ethics' = '#462A09',
                               'Traditional Ecological Knowledge' = '#A39584'))

pg = plot_grid(p1, p2, p3,
               nrow = 1, labels = c('A', 'B', 'C'), vjust = 4.9, label_size = 18)

leg = plot_grid(get_legend(leg_1), 
                get_legend(leg_2), 
                get_legend(leg_3), 
                get_legend(leg_4),
                nrow = 1)

pg_fin = plot_grid(pg + theme(plot.margin = unit(c(0, 0, -1.5, 0), 'in')),
                   leg, nrow = 2, rel_heights = c(1, 0.5))

#ggsave(pg_fin, filename = 'Plots/Figure3_pie.jpeg', 
#       width = 10.3, height = 7.4, units = 'in')

#ggsave(pg_fin, filename = 'Plots/Figure2_pie.jpeg',
#       width = 13.2, height = 7, units = 'in')

ggsave(pg_fin, filename = 'Plots/Figure2.pdf',
       width = 13.2, height = 7, units = 'in')

#ggsave(pg_fin, filename = 'Plots/Figure2.eps',
#       width = 13.2, height = 7, units = 'in', device = 'eps')
#### Figure 3 ####

rm(list = ls())

# Load data
load('Data/cleaned_data_EF_course.RData')
EF_data = data
load('Data/cleaned_data.RData')
course_data = data
rm(data)

## Forecasting Course by Institution Type ##

B_count = 1 # number of baccalaureate school courses
R1_count = 7 # number of R1 school courses

EF_data = EF_data %>%
  filter(Carnegie.Classification.2 != 'NA') %>%
  mutate(Carnegie.Classification.2 = as.factor(Carnegie.Classification.2))

EF_data_count = EF_data %>%
  group_by(Sub.topic, Carnegie.Classification.2) %>%
  dplyr::count()

EF_data_count = EF_data_count %>%
  mutate(n_stan = if_else(Carnegie.Classification.2 == 'B', n / B_count, n / R1_count))

EF_data_count$Sub.topic = factor(EF_data_count$Sub.topic,
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
                                            'Social Science',
                                            'Workflows & Open Science',
                                            'Traditional Ecological Knowledge',
                                            'Model Assessment',
                                            'Data Assimilation',
                                            'State Space Models'))

p1 = EF_data_count %>%  
  ggplot(aes(x = fct_rev(Sub.topic), y = n_stan, fill = Carnegie.Classification.2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_linedraw() +
  labs(fill = 'Carnegie') +
  ggtitle('Forecasting Courses') +
  xlab('') + ylab('Lessons per Institution') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c('B' = '#A6611A',
                               'A/B' = '#C49666',
                               'A' = '#E1CAB3',
                               'R1' = '#018571',
                               'R2' = '#349D8D',
                               'D/PU' = '#67B6AA',
                               'M1' = '#99CEC6',
                               'M3' = '#CCE7E3',
                               'TC' = '#000000'), drop = F) +
  scale_x_discrete(drop = F)

p2 = EF_data %>%
  filter(Education.Level %in% c('Undergraduate', 'Undergraduate & Graduate')) %>%
  group_by(Sub.topic, Carnegie.Classification.2) %>%
  dplyr::count() %>%
  mutate(Sub.topic = factor(Sub.topic, 
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
                                       'Social Science',
                                       'Workflows & Open Science',
                                       'Traditional Ecological Knowledge',
                                       'Model Assessment',
                                       'Data Assimilation',
                                       'State Space Models'))) %>%
  ggplot(aes(x = fct_rev(Sub.topic), y = n, fill = Carnegie.Classification.2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_linedraw() +
  xlab('') + ylab('Lessons per Institution') +
  labs(fill = '') +
  ggtitle('Undergraduate Forecasting Courses') +
  theme(plot.title = element_text(size = 14, hjust = 0.4),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text.y = element_blank(),
        panel.grid = element_blank()) +
  scale_fill_manual(values = c('B' = '#A6611A',
                               'A/B' = '#C49666',
                               'A' = '#E1CAB3',
                               'R1' = '#018571',
                               'R2' = '#349D8D',
                               'D/PU' = '#67B6AA',
                               'M1' = '#99CEC6',
                               'M3' = '#CCE7E3',
                               'TC' = '#000000'), drop = F) +
  scale_x_discrete(drop = F)

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
                                                'Social Science',
                                                'Workflows & Open Science',
                                                'Traditional Ecological Knowledge',
                                                'Model Assessment',
                                                'Data Assimilation',
                                                'State Space Models'))

course_data_count$Carnegie.classification.2 = factor(course_data_count$Carnegie.classification.2,
                                                     levels = c('A', 'A/B', 'B', 'M3', 'M1', 'D/PU', 
                                                                'R2', 'R1', 'TC'))

p3 = course_data_count %>%
  ggplot(aes(x = fct_rev(Sub.topic), y = n_stan, fill = Carnegie.classification.2)) +
  geom_bar(stat = 'identity') +
  coord_flip() +
  theme_linedraw() +
  xlab('') + ylab('Courses per Institution') +
  ggtitle('Forecasting-Adjacent Courses') +
  labs(fill = '') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        panel.grid = element_blank(),
        legend.position = c(0.75, 0.35)) +
  scale_fill_manual(values = c('B' = '#A6611A',
                               'A/B' = '#C49666',
                               'A' = '#E1CAB3',
                               'R1' = '#018571',
                               'R2' = '#349D8D',
                               'D/PU' = '#67B6AA',
                               'M1' = '#99CEC6',
                               'M3' = '#CCE7E3',
                               'TC' = '#000000'), drop = F) +
  scale_x_discrete(drop = F)

pg = plot_grid(p3, 
               p1 + theme(legend.position = 'none'),
               p2 + theme(legend.position = 'none'), 
               nrow = 1, rel_widths = c(0.5, 0.3, 0.3),
               labels = c('A', 'B', 'C'))
pg
#ggsave(pg, filename = 'Plots/barplot_carnegie_subtopic.jpeg', 
#       width = 14, height = 6, units = 'in')
#ggsave(pg, filename = 'Plots/Figure3.pdf',
#       width = 14, height = 6, units = 'in')

ggsave(pg, filename = 'Plots/Figure3.eps',
       width = 14, height = 6, units = 'in', device = 'eps')

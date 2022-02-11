## This script contains the final plots used to visualize the data
## at each curriculum level
## The figure numbers correspond to the figure numbers of the manuscript
## Supplementary figures are compiled in a separate document

## Authors: AM Willson & H Gallo
## Date modified: 10 February 2022

library(tidyverse)
library(pivottabler)
library(RColorBrewer)
library(cowplot)

#### Figure 3 ####

rm(list = ls())

## Online resources plot ##

# Load online resources data
load('Data/cleaned_data_online.RData')
# Rename to avoid confusion
online_data = data
rm(data)

# Find total number of instances each topic was covered across courses
EF_online_resources_df = online_data %>%
  group_by(Category) %>%
  summarize(n())
colnames(EF_online_resources_df) = c('Topic', 'Number of Resources')
EF_online_resources_df$Topic = as.character(EF_online_resources_df$Topic)

# Barplot
barplot_online_resources <- ggplot(EF_online_resources_df, aes(x = reorder(Topic, `Number of Resources`), y = `Number of Resources`)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Open-access, Online Resources") + 
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
barplot_online_resources

# Pie Chart
# Palette
#pal1 = c("#56ebd3", "#4f28af", "#5cb206", "#8a3454", "#99def9", 
#        "#2f4b4e", "#bfcd8e", "#30408d", "#c6c0fe", "#157a48", 
#        "#fd4e8b", "#2cf52b", "#b163d2", "#2e9cbc", "#dc3c07", 
#        "#f1d438", "#2580fe", "#fea53b")
#pal2 = c(#"#72e5ef", 
#        "#56ebd3", "#274c56", "#abd28d", "#6c218e", "#56c23d", 
#        "#e21c7a", "#1e7b20", "#ca50d3", "#3a91fb", "#584982", 
#        "#d0d2f0", "#2a2bf0", "#c0d122", "#873c1a", "#f6a0ba", 
#        "#d11f0b", "#5d99aa", "#ee983a")
#pal3 = c("#75eab6", "#9d222e", "#b1e632", "#4c3e76", "#adb5f0", 
#        "#7a2f9b", "#00d618", "#ff3eb6", "#56a221", "#b32df9", 
#        "#b1bf81", "#0b5313", "#f597fa", "#1c4c5e", "#14bae1", 
#        "#6b3929", "#faa38c", "#ff1c5d")

# Make ordered factor so pie slieces are in order
EF_online_resources_df$Topic = factor(EF_online_resources_df$Topic,
                                      levels = c('Basics of Forecasting',
                                                 'Workflows & Open Science',
                                                 'Statistical Models',
                                                 'Basics of Ecology',
                                                 'Decision Science',
                                                 'Probability & Uncertainty',
                                                 'Basics of Statistics',
                                                 'Data Assimilation',
                                                 'Model Assessment',
                                                 'Working with Data',
                                                 'Basics of Coding',
                                                 'State Space Models',
                                                 #'Ethics',
                                                 'Mechanistic Models',
                                                 'Data Manipulation',
                                                 'Data Sources',
                                                 'Machine Learning',
                                                 'Data Visualization',
                                                 'Science Communication',
                                                 'Traditional Ecological Knowledge'))

pie_chart_online_resources <- ggplot(EF_online_resources_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  guides(fill = guide_legend(ncol = 2, bycol = T)) +
  labs(title = "Open-access, Online Resources", subtitle = 'n = 227') +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 10))
p1 = pie_chart_online_resources + 
  scale_fill_manual(values = c('Basics of Forecasting' = "#56ebd3",
                               'Workflows & Open Science' = "#274c56",
                               'Statistical Models' = "#abd28d",
                               'Basics of Ecology' = "#6c218e",
                               'Decision Science' = "#56c23d", 
                               'Probability & Uncertainty' = "#e21c7a",
                               'Basics of Statistics' = "#1e7b20",
                               'Data Assimilation' = "#ca50d3",
                               'Model Assessment' = "#3a91fb",
                               'Working with Data' = "#584982", 
                               'Basics of Coding' = "#d0d2f0",
                               'State Space Models' = "#2a2bf0",
                               'Ethics' = "#c0d122",
                               'Mechanistic Models' = "#873c1a",
                               'Data Manipulation' = "#f6a0ba", 
                               'Data Sources' = "#d11f0b",
                               'Machine Learning' = "#5d99aa",
                               'Data Visualization' = '#F0EAD6',
                               'Science Communication' = "#ee983a",
                               'Traditional Ecological Knowledge' = "#000000"))

## Forecasting Courses Plot ##

# Load forecasting course data
load('Data/cleaned_data_EF_course.RData')
# Rename to avoid confusion
ef_course_data = data
rm(data)

# Find total number of instances each topic was covered across courses
EF_course_data_df = ef_course_data %>%
  group_by(Sub.topic) %>%
  summarize(n())
colnames(EF_course_data_df) = c('Topic', 'Number of Resources')

# Barplot
barplot_EF_courses <- ggplot(EF_course_data_df, aes(x = reorder(Topic, `Number of Resources`), y = `Number of Resources`)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Forecasting Course Lessons") + 
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
barplot_EF_courses

# Make ordered factor to order topics
ordered = EF_course_data_df %>%
  arrange(desc(`Number of Resources`))
ordered$Topic = factor(ordered$Topic, levels = ordered$Topic)
ordered = rbind(ordered, NA)
ordered$Topic = as.character(ordered$Topic)
ordered[18, 1] = 'Science Communication'
ordered[18, 2] = 0
ordered = rbind(ordered, NA)
ordered[19, 1] = 'Traditional Ecological Knowledge'
ordered[19, 2] = 0
ordered$Topic = as.factor(ordered$Topic)

EF_course_data_df$Topic = factor(EF_course_data_df$Topic, 
                                 levels = c('Basics of Forecasting',
                                                     'Workflows & Open Science',
                                                     'Statistical Models',
                                                     'Basics of Ecology',
                                                     'Decision Science',
                                                     'Probability & Uncertainty',
                                                     'Basics of Statistics',
                                                     'Data Assimilation',
                                                     'Model Assessment',
                                                     'Working with Data',
                                                     'Basics of Coding',
                                                     'State Space Models',
                                                     #'Ethics',
                                                     'Mechanistic Models',
                                                     'Data Manipulation',
                                                     'Data Sources',
                                                     'Machine Learning'))
                                                     #'Data Visualization',
                                                     #'Science Communication',
                                                     #'Traditional Ecological Knowledge'))

pie_chart_EF_courses <- ggplot(EF_course_data_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  guides(fill = guide_legend(ncol = 2, bycol = T)) +
  labs(title = "Forecasting Course Lessons",
       subtitle = 'n = 129') +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
p2 = pie_chart_EF_courses + 
  scale_fill_manual(values = c('Basics of Forecasting' = "#56ebd3",
                               'Workflows & Open Science' = "#274c56",
                               'Statistical Models' = "#abd28d",
                               'Basics of Ecology' = "#6c218e",
                               'Decision Science' = "#56c23d", 
                               'Probability & Uncertainty' = "#e21c7a",
                               'Basics of Statistics' = "#1e7b20",
                               'Data Assimilation' = "#ca50d3",
                               'Model Assessment' = "#3a91fb",
                               'Working with Data' = "#584982", 
                               'Basics of Coding' = "#d0d2f0",
                               'State Space Models' = "#2a2bf0",
                               'Ethics' = "#c0d122",
                               'Mechanistic Models' = "#873c1a",
                               'Data Manipulation' = "#f6a0ba", 
                               'Data Sources' = "#d11f0b",
                               'Machine Learning' = "#5d99aa",
                               'Data Visualization' = '#F0EAD6',
                               'Science Communication' = "#ee983a",
                               'Traditional Ecological Knowledge' = "#000000"))

## Forecasting-Adjacent Courses Plot ##

# Load forecasting-adjacent course data
load('Data/cleaned_data.RData')
# Rename to avoid confusion
course_data = data
rm(data)

# Find total number of instances each topic was covered across courses
FA_course_data_df = course_data %>%
  group_by(Sub.topic) %>%
  summarize(n())
colnames(FA_course_data_df) = c('Topic', 'Number of Resources')

# Barplot
barplot_FA_courses <- ggplot(FA_course_data_df, aes(x = reorder(Topic, `Number of Resources`), y = `Number of Resources`)) + 
  geom_bar(stat = "identity") + 
  coord_flip() + 
  labs(title = "Forecasting-Adjacent Courses") + 
  xlab("") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))
barplot_FA_courses

# Make ordered factor so pie slieces are in order
FA_course_data_df$Topic = factor(FA_course_data_df$Topic, 
                                 levels = c('Basics of Forecasting',
                                            'Workflows & Open Science',
                                            'Statistical Models',
                                            'Basics of Ecology',
                                            'Decision Science',
                                            'Probability & Uncertainty',
                                            'Basics of Statistics',
                                            #'Data Assimilation',
                                            'Model Assessment',
                                            'Working with Data',
                                            'Basics of Coding',
                                            #'State Space Models',
                                            'Ethics',
                                            'Mechanistic Models',
                                            'Data Manipulation',
                                            'Data Sources',
                                            'Machine Learning',
                                            'Data Visualization',
                                            'Science Communication',
                                            'Traditional Ecological Knowledge'))

pie_chart_FA_courses <- ggplot(FA_course_data_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  guides(fill = guide_legend(ncol = 2, bycol = T)) +
  labs(title = "Forecasting-Adjacent Courses",
       subtitle = 'n = 1,485') +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10))
p3 = pie_chart_FA_courses + 
  scale_fill_manual(values = c('Basics of Forecasting' = "#56ebd3",
                               'Workflows & Open Science' = "#274c56",
                               'Statistical Models' = "#abd28d",
                               'Basics of Ecology' = "#6c218e",
                               'Decision Science' = "#56c23d", 
                               'Probability & Uncertainty' = "#e21c7a",
                               'Basics of Statistics' = "#1e7b20",
                               'Data Assimilation' = "#ca50d3",
                               'Model Assessment' = "#3a91fb",
                               'Working with Data' = "#584982", 
                               'Basics of Coding' = "#d0d2f0",
                               'State Space Models' = "#2a2bf0",
                               'Ethics' = "#c0d122",
                               'Mechanistic Models' = "#873c1a",
                               'Data Manipulation' = "#f6a0ba", 
                               'Data Sources' = "#d11f0b",
                               'Machine Learning' = "#5d99aa",
                               'Data Visualization' = '#F0EAD6',
                               'Science Communication' = "#ee983a",
                               'Traditional Ecological Knowledge' = "#000000"))

# Combine plots into multi-panel figure

# Function to extract legend
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

# Plot figures together (bar plots)
pg = plot_grid(barplot_online_resources, 
               barplot_EF_courses,
               barplot_FA_courses, nrow = 3,
               labels = c('A', 'B', 'C'))

ggsave(pg, filename = 'Plots/Figure3_bar.jpeg', 
       width = 5.2, height = 10, units = 'in')

# Plot figures together (pie chart)
leg = get_legend(p1)

pg = plot_grid(p1 + theme(legend.position = 'none'),
          p2 + theme(legend.position = 'none'),
          p3 + theme(legend.position = 'none'),
          nrow = 1, labels = c('A', 'B', 'C'), vjust = 10.6)

# Plot with legend
pg_fin = plot_grid(pg + theme(plot.margin = unit(c(0, 0, -1.5, 0), 'in')),
                   leg, 
                   nrow = 2,
                   rel_heights = c(1, 0.5))
pg_fin

ggsave(pg_fin, filename = 'Plots/Figure3_pie.jpeg', 
       width = 10.3, height = 7.4, units = 'in')

#### Figure 4 ####
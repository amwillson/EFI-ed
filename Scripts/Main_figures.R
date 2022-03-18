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
  dplyr::summarize(n())
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

# Make ordered factor so pie slices are in order
EF_online_resources_df$Topic = factor(EF_online_resources_df$Topic, 
                                      levels = c('Basics of Forecasting',
                                                 'Statistical Models',
                                                 'Workflows & Open Science',
                                                 'Probability & Uncertainty',
                                                 'Data Assimilation',
                                                 'Social Science',
                                                 'State Space Models',
                                                 'Model Assessment',
                                                 'Basics of Statistics',
                                                 'Working with Data',
                                                 'Basics of Coding',
                                                 'Basics of Ecology',
                                                 'Data Manipulation',
                                                 'Mechanistic Models',
                                                 'Machine Learning',
                                                 'Data Visualization',
                                                 'Data Sources',
                                                 'Science Communication',
                                                 'Traditional Ecological Knowledge'))

pie_chart_online_resources <- ggplot(EF_online_resources_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  guides(fill = guide_legend(ncol = 4, bycol = T)) +
  labs(title = "Open-access, Online Resources", subtitle = 'n = 227') +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))
p1 = pie_chart_online_resources + 
  scale_fill_manual(values = c('Basics of Forecasting' = "#56ebd3",
                               'Statistical Models' = "#274c56",
                               'Workflows & Open Science' = "#abd28d",
                               'Probability & Uncertainty' = "#6c218e",
                               'Data Assimilation' = "#56c23d", 
                               'Social Science' = "#e21c7a",
                               'State Space Models' = "#1e7b20",
                               'Model Assessment' = "#ca50d3",
                               'Basics of Statistics' = "#3a91fb",
                               'Working with Data' = "#584982", 
                               'Basics of Coding' = "#d0d2f0",
                               'Basics of Ecology' = "#2a2bf0",
                               'Data Manipulation' = "#c0d122",
                               'Ethics' = "#873c1a",
                               'Mechanistic Models' = "#f6a0ba", 
                               'Machine Learning' = "#d11f0b",
                               'Data Visualization' = "#5d99aa",
                               'Data Sources' = '#F0EAD6',
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
  dplyr::summarize(n())
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

EF_course_data_df$Topic = factor(EF_course_data_df$Topic, 
                                 levels = c('Basics of Forecasting',
                                            'Statistical Models',
                                            'Workflows & Open Science',
                                            'Probability & Uncertainty',
                                            'Data Assimilation',
                                            'Social Science',
                                            'State Space Models',
                                            'Model Assessment',
                                            'Basics of Statistics',
                                            'Working with Data',
                                            'Basics of Coding',
                                            'Basics of Ecology',
                                            'Data Manipulation',
                                            'Ethics',
                                            'Mechanistic Models',
                                            'Machine Learning',
                                            'Data Visualization',
                                            'Data Sources'))

pie_chart_EF_courses <- ggplot(EF_course_data_df, aes(x = "", y = `Number of Resources`, fill = Topic)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_polar("y", start = 0) + 
  guides(fill = guide_legend(ncol = 2, bycol = T)) +
  labs(title = "Forecasting Course Lessons",
       subtitle = 'n = 192') +
  theme_void() +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12))
p2 = pie_chart_EF_courses + 
  scale_fill_manual(values = c('Basics of Forecasting' = "#56ebd3",
                               'Statistical Models' = "#274c56",
                               'Workflows & Open Science' = "#abd28d",
                               'Probability & Uncertainty' = "#6c218e",
                               'Data Assimilation' = "#56c23d", 
                               'Social Science' = "#e21c7a",
                               'State Space Models' = "#1e7b20",
                               'Model Assessment' = "#ca50d3",
                               'Basics of Statistics' = "#3a91fb",
                               'Working with Data' = "#584982", 
                               'Basics of Coding' = "#d0d2f0",
                               'Basics of Ecology' = "#2a2bf0",
                               'Data Manipulation' = "#c0d122",
                               'Ethics' = "#873c1a",
                               'Mechanistic Models' = "#f6a0ba", 
                               'Machine Learning' = "#d11f0b",
                               'Data Visualization' = "#5d99aa",
                               'Data Sources' = '#F0EAD6',
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
  dplyr::summarize(n())
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
                                            'Statistical Models',
                                            'Workflows & Open Science',
                                            'Probability & Uncertainty',
                                            'Social Science',
                                            'Model Assessment',
                                            'Basics of Statistics',
                                            'Working with Data',
                                            'Basics of Coding',
                                            'Basics of Ecology',
                                            'Data Manipulation',
                                            'Ethics',
                                            'Mechanistic Models',
                                            'Machine Learning',
                                            'Data Visualization',
                                            'Data Sources',
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
        legend.text = element_text(size = 12))
p3 = pie_chart_FA_courses + 
  scale_fill_manual(values = c('Basics of Forecasting' = "#56ebd3",
                               'Statistical Models' = "#274c56",
                               'Workflows & Open Science' = "#abd28d",
                               'Probability & Uncertainty' = "#6c218e",
                               'Data Assimilation' = "#56c23d", 
                               'Social Science' = "#e21c7a",
                               'State Space Models' = "#1e7b20",
                               'Model Assessment' = "#ca50d3",
                               'Basics of Statistics' = "#3a91fb",
                               'Working with Data' = "#584982", 
                               'Basics of Coding' = "#d0d2f0",
                               'Basics of Ecology' = "#2a2bf0",
                               'Data Manipulation' = "#c0d122",
                               'Ethics' = "#873c1a",
                               'Mechanistic Models' = "#f6a0ba", 
                               'Machine Learning' = "#d11f0b",
                               'Data Visualization' = "#5d99aa",
                               'Data Sources' = '#F0EAD6',
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
                                 levels = c('Basics of Forecasting',
                                            'Workflows & Open Science',
                                            'Statistical Models',
                                            'Basics of Ecology',
                                            'Probability & Uncertainty',
                                            'Basics of Statistics',
                                            'Working with Data',
                                            'Data Assimilation',
                                            'State Space Models',
                                            'Social Science',
                                            'Basics of Coding',
                                            'Model Assessment',
                                            'Mechanistic Models',
                                            'Data Sources',
                                            'Data Manipulation',
                                            'Ethics',
                                            'Data Visualization'))

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
  scale_fill_manual(values = c('B' = '#dfc27d', 'R1' = '#35978f'))

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
                                                'Model Assessment'))

course_data_count$Carnegie.classification.2 = factor(course_data_count$Carnegie.classification.2,
                                                     levels = c('A', 'A/B', 'B', 'M3', 'M1', 'D/PU', 
                                                                'R2', 'R1', 'TC'))

p2 = course_data_count %>%
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
        axis.text = element_text(size = 10)) +
  scale_fill_manual(values = c('#8c510a',
                               '#bf812d',
                               '#dfc27d',
                               '#f6e8c3',
                               '#f5f5f5',
                               '#c7eae5',
                               '#80cdc1',
                               '#35978f',
                               '#01665e'))

pg = plot_grid(p1 + theme(legend.position = 'none'), p2, 
               nrow = 1, rel_widths = c(0.5, 0.65),
               labels = c('A', 'B'))
pg
ggsave(pg, filename = 'Plots/barplot_carnegie_subtopic.jpeg', 
       width = 12, height = 6, units = 'in')

#### Figure 5 ####

rm(list = ls())

## Forecasting-Adjacent Courses Map ##

# Load in data
College_Locations <- read_excel('Data/College Location Data.xlsx', sheet  = 'College Location Data Final')
load("Data/cleaned_data.RData")
ef.data <- data.frame(data)

# Organize course data
pt <- PivotTable$new() # create pivottable 
pt$addData(ef.data) #populate with ef.data
pt$addColumnDataGroups("Sub.topic") #using Sub-Topic data to populate pivottable
pt$addRowDataGroups("College")
pt$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()") #calculating number of courses
pt$renderPivot() #create pivot table 
summary_subtopic <- pt$asDataFrame() #pushing pivottable to dataframe for easier use and analysis later

summary_subtopic <- summary_subtopic %>% 
  mutate(across(everything(), .fns = ~replace_na(.,0))) #put zeros in for all blank cells

row_names_to_remove <- c("Total") #had to remove total row because it was taking mean including total row
summary_subtopic_2 <- summary_subtopic[!(row.names(summary_subtopic) %in% row_names_to_remove),] #push to new dataset

reduced_data <- dplyr::select(ef.data, -c(`Sub.topic`)) #removing columns from dataset
reduced_data <- distinct(reduced_data, College, .keep_all = TRUE) #this removes all duplicate rows based on college name, so now there should the same number of rows as distinct colleges

data_for_regression <- merge(data.frame(reduced_data, row.names=NULL), data.frame(summary_subtopic_2, row.names=NULL), 
                             by = 0, all = TRUE)[-1] #merge another dataset that is easier to use for the regression/BCLM

course_total <- data.frame(summary_subtopic_2$Total)
transformed_data <- usmap_transform(College_Locations)

# Make weighted data according to number of courses offered
weighted.data <- cbind(transformed_data, course_total)

sort.df <- with(data_for_regression,  data_for_regression[order(College) , ])
Type <- data.frame(sort.df$Type)
weighted.data <- weighted.data[order(weighted.data$College) , ]
type_weighted_data <- cbind(weighted.data, Type)

type_weighted_data <-type_weighted_data %>% 
  dplyr::rename('Total Courses'= summary_subtopic_2.Total, Type = sort.df.Type)

# Unweighted map, locations only
map_locations <- plot_usmap("states") + 
  geom_point(data = transformed_data, aes(x = Longitude.1, y = Latitude.1), color = "#177E89", size = 1.2, shape = 8, stroke = 0.9) +
  ggtitle('Forecasting-Adjacent Courses') +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
map_locations
ggsave(map_locations, filename = 'Plots/map_location_only.jpeg')

# Weighted map with institution type
map_weighted_data <- plot_usmap("states") + 
  geom_point(data = type_weighted_data, aes(x = Longitude.1, y = Latitude.1, size = `Total Courses`, color = Type)) +
  theme(legend.position = "left") + 
  labs(title = "Forecasting-Adjacent Courses") +
  scale_color_manual(values = c("#41bbc5", 
                                "#2c457d", 
                                "#b2b2f9", 
                                "#88075f", 
                                "#3db366")) +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10))
map_weighted_data
ggsave(map_weighted_data, filename = 'Plots/map_weighted_inst.jpeg')

# Weighted map without institution type
map_weighted_noinst <- plot_usmap("states") + 
  geom_point(data = type_weighted_data, aes(x = Longitude.1, y = Latitude.1, size = `Total Courses`), color = '#197d89', alpha = 0.8) +
  theme(legend.position = "left") + 
  labs(title = "Forecasting-Adjacent Courses") +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10))
map_weighted_noinst
ggsave(map_weighted_noinst, filename = 'Plots/map_weighted_noinst.jpeg')

## Forecasting Courses Map ##

# Load in data
load('Data/cleaned_data_EF_course.RData')
EF_courses = data
EF_course_location <- read_excel('Data/College Location Data.xlsx', sheet = 'EF Courses')

EF_location_transformed <- usmap_transform(EF_course_location)

# Unweighted map, location only
EF_location <- plot_usmap('states') +
  geom_point(data = EF_location_transformed, aes(x = Longitude.1, y = Latitude.1), color = '#4fdf96', size = 2, shape = 4, stroke = 1.2) +
  ggtitle('Forecasting Courses') +
  theme(plot.title = element_text(size = 14, hjust = 0.5))
EF_location
ggsave(EF_location, filename = 'Plots/map_location_only_EF.jpeg')

# Unweighted map with institution type
EF_location_visual <- plot_usmap("states") + 
  geom_point(data = EF_location_transformed, aes(x = Longitude.1, y = Latitude.1, color = Type), shape = 4, size = 2, stroke = 1.2) + 
  labs(title = "Forecasting Courses") + 
  scale_color_manual(values = c("#41bbc5", 
                                "#2c457d", 
                                "#b2b2f9", 
                                "#88075f", 
                                "#3db366")) +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.position = c(1, 0.5))
EF_location_visual
ggsave(EF_location_visual, filename = 'Plots/map_EF_inst.jpeg',
       width = 11, height = 6, units = 'in')

## Plot together ##

# Prepare dataframe to merge forecasting and forecasting-adjacent
transformed_type = cbind(transformed_data, Type)
transformed_type = cbind(transformed_type, rep('Forecasting-Adjacent', nrow(transformed_type)))
colnames(transformed_type)[8] = 'Curriculum Level'
colnames(transformed_type)[7] = 'Type'

# Prepare dataframe to merge forecasting and forecasting-adjacent
EF_location_transformed = cbind(EF_location_transformed, rep('Forecasting', nrow(EF_location_transformed)))
colnames(EF_location_transformed)[8] = 'Curriculum Level'

full_transformed = transformed_type %>%
  full_join(EF_location_transformed) %>%
  mutate(Type = recode(Type, 
                       '4-year private' = '4-Year Private',
                       '4-year public' = '4-Year Public'))

# Unweighted map with no institution type, both curriculum levels
both_location <- plot_usmap('states') +
  geom_point(data = full_transformed, aes(x = Longitude.1, y = Latitude.1, fill = `Curriculum Level`, size = `Curriculum Level`, shape = `Curriculum Level`)) +
  labs(title = 'Forecasting and Forecasting-Adjacent Courses') +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        legend.title = element_text(size = 14, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.position = c(0.9, 0.5)) +
  scale_fill_manual(values = c('Forecasting' = '#177E89', 'Forecasting-Adjacent' = '#72e5ef')) +
  scale_size_manual(values = c('Forecasting' = 2.5, 'Forecasting-Adjacent' = 2.5)) +
  scale_shape_manual(values = c('Forecasting' = 24, 'Forecasting-Adjacent' = 21))
both_location
ggsave(both_location, filename = 'Plots/map_both_noinst.jpeg', 
       width = 11.9, height = 6, units = 'in')

# Unweighted map with institution type, both curriculum levels
both_location_inst <- plot_usmap('states') +
  geom_point(data = full_transformed, aes(x = Longitude.1, y = Latitude.1, color = Type, size = `Curriculum Level`, shape = `Curriculum Level`), stroke = 1.4) +
  labs(title = 'Forecasting and Forecasting-Adjacent Courses') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.position = c(0.95, 0.4)) +
  scale_color_manual(values = c("#41bbc5", 
                                "#2c457d", 
                                "#b2b2f9", 
                                "#88075f", 
                                "#3db366")) +
  scale_size_manual(values = c('Forecasting-Adjacent' = 2.5, 'Forecasting' = 2.5)) +
  scale_shape_manual(values = c('Forecasting-Adjacent' = 16, 'Forecasting' = 4))
both_location_inst
ggsave(both_location_inst, filename = 'Plots/both_location_inst.jpeg')

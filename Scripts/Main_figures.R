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
  guides(fill = guide_legend(ncol = 4, bycol = T)) +
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
  geom_point(data = full_transformed, aes(x = Longitude.1, y = Latitude.1, color = `Curriculum Level`, size = `Curriculum Level`, shape = `Curriculum Level`), stroke = 1.4) +
  labs(title = 'Forecasting and Forecasting-Adjacent Courses') +
  theme(plot.title = element_text(size = 14, hjust = 0.5),
        legend.title = element_text(size = 12, hjust = 0.5),
        legend.text = element_text(size = 10),
        legend.position = c(0.9, 0.5)) +
  scale_color_manual(values = c('Forecasting' = '#177E89', 'Forecasting-Adjacent' = '#72e5ef')) +
  scale_size_manual(values = c('Forecasting' = 2.5, 'Forecasting-Adjacent' = 2.5)) +
  scale_shape_manual(values = c('Forecasting' = 4, 'Forecasting-Adjacent' = 16))
both_location
ggsave(both_location, filename = 'Plots/map_both_noinst.jpeg', 
       width = 10, height = 6, units = 'in')

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

library(plyr)
library(caret)
library(dplyr)
library(basictabler)
library(flextable)
library(pivottabler)
library(tidyr)
library(janitor)
library(tidyverse)
library(nnet)
library(ROCR)
library(MASS)
library(forecast)
library(generalhoslem)
library(data.table)
library(RColorBrewer)
library(viridis)
library(readxl)
library(usmap)
library(ggplot2)
library(stringr)
library(gtsummary)
library(cowplot)

# Clear environment
rm(list = ls())


EF_courses <- read_excel('EF_Courses_1_course_removed.xlsx')
EF_courses <- data.frame(EF_courses)
EF_courses_subtopics <- PivotTable$new() # create pivottable 
EF_courses_subtopics$addData(EF_courses) #populate with ef.data
EF_courses_subtopics$addColumnDataGroups("Sub.topic", addTotal = FALSE) #using Sub-Topic data to populate pivottable
EF_courses_subtopics$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
EF_courses_subtopics$renderPivot() #create pivot table 
EF_courses_subtopics <- EF_courses_subtopics$asDataFrame()
colnames_EF_courses <- data.frame(colnames(EF_courses_subtopics))
EF_courses_subtopics <- transpose(EF_courses_subtopics)
EF_courses_subtopics <- cbind(EF_courses_subtopics, colnames_EF_courses)
EF_courses_subtopics <-EF_courses_subtopics %>% dplyr::rename('Subtopics'= colnames.EF_courses_subtopics., 'Number of Courses' = V1)
EF_course_topic_distribution <- ggplot(EF_courses_subtopics, aes(x="", y=`Number of Courses`, fill=Subtopics)) + geom_bar(stat="identity", width=1, color = "black") + coord_polar("y", start=0) + theme_void() + labs(title = "EF Course Topic Distribution")
EF_course_topic_distribution
barplot_subtopic_distribution <- ggplot(EF_courses_subtopics, aes(x=reorder(Subtopics,`Number of Courses`), y=`Number of Courses`)) + geom_bar(position="stack", stat = "identity") + coord_flip() + labs(title = "Topic Distribution Undergraduate EF Courses") + xlab("Subtopics") + ylab("Number of Occurrences")
barplot_subtopic_distribution


color_count <- length(EF_courses_subtopics$Subtopics)
coul <- brewer.pal(12, "Paired")
coul <- colorRampPalette(coul)(color_count)
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))

set.seed(2643598)                                             # Set random seed
palette3 <- sample(palette3_all, color_count)                    # Sample color

EF_course_topic_distribution + scale_fill_manual(values = coul)
EF_course_topic_distribution + scale_fill_manual(values = palette3)

distribution_percent <- EF_courses_subtopics %>% mutate(Percentage = EF_courses_subtopics$`Number of Courses`/sum(EF_courses_subtopics$`Number of Courses`))
distribution_percent <- distribution_percent %>% mutate(across(where(is.numeric), ~ round(., 2)))
distribution_percent

only_undergrad <- subset(EF_courses, Education.Level!="Graduate")
EF_courses_undergrad <- PivotTable$new() # create pivottable 
EF_courses_undergrad$addData(only_undergrad) #populate with ef.data
EF_courses_undergrad$addColumnDataGroups("Sub.topic", addTotal = FALSE) #using Sub-Topic data to populate pivottable
EF_courses_undergrad$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
EF_courses_undergrad$renderPivot() #create pivot table 
EF_courses_undergrad <- EF_courses_undergrad$asDataFrame()
colnames_EF_undergrad <- data.frame(colnames(EF_courses_undergrad))
EF_courses_undergrad <- transpose(EF_courses_undergrad)
EF_courses_undergrad <- cbind(EF_courses_undergrad, colnames_EF_undergrad)
EF_courses_undergrad <-EF_courses_undergrad %>% dplyr::rename('Subtopics'= colnames.EF_courses_undergrad., 'Number of Courses' = V1)
EF_course_topic_distribution_undergrad <- ggplot(EF_courses_undergrad, aes(x="", y=`Number of Courses`, fill=Subtopics)) + geom_bar(stat="identity", width=1, color = "black") + coord_polar("y", start=0) + theme_void() + labs(title = "EF Course Topic Distribution")
EF_course_topic_distribution_undergrad + scale_fill_manual(values = palette3)
barplot_subtopic_undergrad <- ggplot(EF_courses_undergrad, aes(x=reorder(Subtopics,`Number of Courses`), y=`Number of Courses`)) + geom_bar(position="stack", stat = "identity") + coord_flip() + labs(title = "Topic Distribution Undergraduate EF Courses") + xlab("Subtopics") + ylab("Number of Occurrences")
barplot_subtopic_undergrad

only_graduate <- subset(EF_courses, Education.Level!="Undergraduate")
EF_courses_graduate <- PivotTable$new() # create pivottable 
EF_courses_graduate$addData(only_graduate) #populate with ef.data
EF_courses_graduate$addColumnDataGroups("Sub.topic", addTotal = FALSE) #using Sub-Topic data to populate pivottable
EF_courses_graduate$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
EF_courses_graduate$renderPivot() #create pivot table 
EF_courses_graduate <- EF_courses_graduate$asDataFrame()
colnames_EF_graduate <- data.frame(colnames(EF_courses_graduate))
EF_courses_graduate <- transpose(EF_courses_graduate)
EF_courses_graduate <- cbind(EF_courses_graduate, colnames_EF_graduate)
EF_courses_graduate <-EF_courses_graduate %>% dplyr::rename('Subtopics'= colnames.EF_courses_graduate., 'Number of Courses' = V1)
EF_course_topic_distribution_graduate <- ggplot(EF_courses_graduate, aes(x="", y=`Number of Courses`, fill=Subtopics)) + geom_bar(stat="identity", width=1, color = "black") + coord_polar("y", start=0) + theme_void() + labs(title = "EF Course Topic Distribution")
EF_course_topic_distribution_graduate + scale_fill_manual(values = palette3)
barplot_subtopic_graduate <- ggplot(EF_courses_graduate, aes(x=reorder(Subtopics,`Number of Courses`), y=`Number of Courses`)) + geom_bar(position="stack", stat = "identity") + coord_flip() + labs(title = "Topic Distribution Undergraduate EF Courses") + xlab("Subtopics") + ylab("Number of Occurrences")
barplot_subtopic_graduate


###Subtopic distribution large dataset###

load("cleaned_data.RData")
# Courses_by_school <- read_excel('Courses_by_school_final.xlsx', sheet = 'Combined_Final')
# 
ef.data <- data.frame(data) #push data to dataframe

pt <- PivotTable$new() # create pivottable 
pt$addData(ef.data) #populate with ef.data
pt$addColumnDataGroups("Sub.topic") #using Sub-Topic data to populate pivottable
pt$addRowDataGroups("College")
pt$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
pt$renderPivot() #create pivot table 
summary_subtopic <- pt$asDataFrame() #pushing pivottable to dataframe for easier use and analysis later
summary_subtopic

summary_subtopic <- summary_subtopic %>% mutate(across(everything(), .fns = ~replace_na(.,0))) #put zeros in for all blank cells

row_names_to_remove<-c("Total") #had to remove total row because it was taking mean including total row
summary_subtopic_2 <- summary_subtopic[!(row.names(summary_subtopic) %in% row_names_to_remove),] #push to new dataset
mean_by_subtopic <- summary_subtopic_2 %>% summarise_if(is.numeric, mean) #calculate mean across all columns without total row
sd_by_subtopic <- summary_subtopic_2 %>% summarise_if(is.numeric, sd) #calculate sd across all rows

rownames(mean_by_subtopic) <- ("Mean") #renaming row name
rownames(sd_by_subtopic) <- ("sd") #renaming row name
sd_mean_subtopics <- rbind(summary_subtopic, mean_by_subtopic, sd_by_subtopic) #merge the the datasets/vectors
is.num <- sapply(sd_mean_subtopics, is.numeric) #
sd_mean_subtopics[is.num] <- lapply(sd_mean_subtopics[is.num], round, 2) #rounding all values in dataset to 2 decimals
#now going to add relevant variables back in like state, school classification, highest degree offered etc.
reduced_data <- dplyr::select(ef.data, -c(`Sub.topic`)) #removing columns from dataset
reduced_data <- distinct(reduced_data, College, .keep_all = TRUE) #this removes all duplicate rows based on college name, so now there should the same number of rows as distinct colleges

numeric_type <- data.frame(reduced_data$Type) 
must_convert<-sapply(numeric_type,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(numeric_type[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out<-cbind(numeric_type[,!must_convert],M2)
dff <- merge(data.frame(reduced_data, row.names=NULL), data.frame(sd_mean_subtopics, row.names=NULL), 
             by = 0, all = TRUE)[-1] #merging dataframes to create full dataframe with college data and summarized subtopic data
data_for_regression <- merge(data.frame(reduced_data, row.names=NULL), data.frame(summary_subtopic_2, row.names=NULL), 
                             by = 0, all = TRUE)[-1] #merge another dataset that is easier to use for the regression/BCLM



###Full dataset map###
College_Locations <- read_excel('College Location Data.xlsx', sheet  = 'College Location Data Final')

course_total <- data.frame(summary_subtopic_2$Total)
transformed_data <- usmap_transform(College_Locations)
map_locations <- plot_usmap("states") + geom_point(data = transformed_data, aes(x = Longitude.1, y = Latitude.1), color = "red", size = 1)
map_locations
weighted.data <- cbind(transformed_data, course_total)

sort.df <- with(data_for_regression,  data_for_regression[order(College) , ])
Type <- data.frame(sort.df$Type)
weighted.data <- weighted.data[order(weighted.data$College) , ]
type_weighted_data <- cbind(weighted.data, Type)

type_weighted_data <-type_weighted_data %>% dplyr::rename('Total Courses'= summary_subtopic_2.Total, Type = sort.df.Type)

map_weighted_data <- plot_usmap("states") + geom_point(data = type_weighted_data, aes(x = Longitude.1, y = Latitude.1, size=`Total Courses`, color = Type))
map_weighted_data
map_nonweighted_type <- plot_usmap("states") + geom_point(data = type_weighted_data, aes(x = Longitude.1, y = Latitude.1, color = Type))
map_weighted_data
map_nonweighted_type
us_scatter_type_courses_weighted <- map_weighted_data + theme(legend.position = "left") + labs(title = "Courses by College Type")
us_scatter_type_courses_unweighted <- map_nonweighted_type + theme(legend.position = "left") + labs(title = "Courses by College Type")
us_scatter_type_courses_weighted <- us_scatter_type_courses_weighted + scale_color_manual(values = c("2-Year Technical" = "red", "4-Year Private" = "orange", "4-Year Public" = "green", "Community College" = "light blue", "For Profit" = "purple"))
us_scatter_type_courses_unweighted <- us_scatter_type_courses_unweighted + scale_color_manual(values = c("2-Year Technical" = "red", "4-Year Private" = "orange", "4-Year Public" = "green", "Community College" = "light blue", "For Profit" = "purple"))
us_scatter_type_courses_weighted

###Map Visualization for EF Courses###
unique(EF_courses[c("Institution")])

EF_course_location <- read_excel('College Location Data.xlsx', sheet = 'EF Courses')
EF_location_transformed <- usmap_transform(EF_course_location)
EF_location_visual <- plot_usmap("states") + geom_point(data = EF_location_transformed, aes(x = Longitude.1, y = Latitude.1, color = Type))+ labs(title = "EF Courses by Institution Type")+ scale_color_manual(values = c("2-year vocational/technical school" = "red", "4-year private" = "orange", "4-year public" = "green", "Community College" = "light blue", "for profit college" = "purple"))
EF_location_visual_no_legend <-EF_location_visual + theme(legend.position = "none")


weighted_vs_EF_courses <- plot_grid(us_scatter_type_courses_weighted, EF_location_visual_no_legend, scale = c(1, .65))
unweighted_vs_EF_courses <- plot_grid(us_scatter_type_courses_unweighted, EF_location_visual_no_legend, scale = c(1, .65))
unweighted_vs_EF_courses
weighted_vs_EF_courses


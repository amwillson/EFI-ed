# load all necessary libraries
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
library(plotly)

# Clear environment
rm(list = ls())
load("cleaned_data.RData")
ef.data <- data.frame(data) #push data to dataframe

ef.data <- data
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
reduced_data <- reduced_data %>% arrange(College)
numeric_type <- data.frame(reduced_data$Type) 
must_convert<-sapply(numeric_type,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(numeric_type[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric

out<-cbind(numeric_type[,!must_convert],M2)
nrow_sd_mean_sub <- nrow(sd_mean_subtopics)
rows_remove <- seq(nrow_sd_mean_sub-2, nrow_sd_mean_sub)
sd_mean_subtopics <- sd_mean_subtopics %>% filter(!row_number() %in% rows_remove)
dff <- merge(data.frame(reduced_data, row.names=NULL), data.frame(sd_mean_subtopics, row.names=NULL), 
             by = 0, all = TRUE)[-1] #merging dataframes to create full dataframe with college data and summarized subtopic data
data_for_regression <- merge(data.frame(reduced_data, row.names=NULL), data.frame(summary_subtopic_2, row.names=NULL), 
                             by = 0, all = TRUE)[-1] #merge another dataset that is easier to use for the regression/BCLM

data_for_regression <- data_for_regression %>% arrange(College)


College_Locations <- read_excel('College Location Data.xlsx', sheet  = 'College Location Data Final')

course_total <- data.frame(summary_subtopic_2$Total)
transformed_data <- usmap_transform(College_Locations)
transformed_data <- transformed_data %>% arrange(College)
weighted.data <- cbind(transformed_data, course_total)

sort.df <- with(data_for_regression,  data_for_regression[order(College) , ])
Type <- data.frame(sort.df$Type)
weighted.data <- weighted.data[order(weighted.data$College) , ]
type_weighted_data <- cbind(weighted.data, Type)

type_weighted_data <-type_weighted_data %>% dplyr::rename('Total Courses'= summary_subtopic_2.Total, Type = sort.df.Type)

map_weighted_data <- plot_usmap("states") + geom_point(data = type_weighted_data, aes(x = Longitude.1, y = Latitude.1, size=`Total Courses`, color = Type))
map_weighted_data
us_scatter_type_courses <- map_weighted_data + theme(legend.position = "left") + labs(title = "Courses by College Type")
us_scatter_type_courses #finalized weighted map

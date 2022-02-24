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

dff <- dff %>% arrange(College) #arranging rows in alphabetical order by college name 

### Map Visualization By Region ###
College_Locations <- read_excel('College Location Data.xlsx', sheet  = 'College Location Data Final')

course_total <- data.frame(summary_subtopic_2$Total)
transformed_data <- usmap_transform(College_Locations)
transformed_data <- transformed_data %>% arrange(College)
weighted.data <- cbind(transformed_data, course_total)
college_state <- data.frame(dff$State)
weighted_data_state <- cbind(weighted.data, college_state)

weighted_data_state$dff.State <- as.character(weighted_data_state$dff.State)

weighted_data_state <- weighted_data_state %>%
  mutate(Status = case_when(
    endsWith(dff.State, "CT") ~ "Northeast",
    endsWith(dff.State, "ME") ~ "Northeast",
    endsWith(dff.State, "MA") ~ "Northeast",
    endsWith(dff.State, "NH") ~ "Northeast",
    endsWith(dff.State, "RI") ~ "Northeast",
    endsWith(dff.State, "VT") ~ "Northeast",
    endsWith(dff.State, "NJ") ~ "Northeast",
    endsWith(dff.State, "NY") ~ "Northeast",
    endsWith(dff.State, "PA") ~ "Northeast",
    endsWith(dff.State, "IN") ~ "Midwest",
    endsWith(dff.State, "IL") ~ "Midwest",
    endsWith(dff.State, "MI") ~ "Midwest",
    endsWith(dff.State, "OH") ~ "Midwest",
    endsWith(dff.State, "WI") ~ "Midwest",
    endsWith(dff.State, "IA") ~ "Midwest",
    endsWith(dff.State, "KS") ~ "Midwest",
    endsWith(dff.State, "MN") ~ "Midwest",
    endsWith(dff.State, "MO") ~ "Midwest",
    endsWith(dff.State, "NE") ~ "Midwest",
    endsWith(dff.State, "ND") ~ "Midwest",
    endsWith(dff.State, "SD") ~ "Midwest",
    endsWith(dff.State, "DE") ~ "South",
    endsWith(dff.State, "DC") ~ "South",
    endsWith(dff.State, "FL") ~ "South",
    endsWith(dff.State, "GA") ~ "South",
    endsWith(dff.State, "MD") ~ "South",
    endsWith(dff.State, "NC") ~ "South",
    endsWith(dff.State, "SC") ~ "South",
    endsWith(dff.State, "VA") ~ "South",
    endsWith(dff.State, "WV") ~ "South",
    endsWith(dff.State, "AL") ~ "South",
    endsWith(dff.State, "KY") ~ "South",
    endsWith(dff.State, "MS") ~ "South",
    endsWith(dff.State, "TN") ~ "South",
    endsWith(dff.State, "AR") ~ "South",
    endsWith(dff.State, "LA") ~ "South",
    endsWith(dff.State, "OK") ~ "South",
    endsWith(dff.State, "TX") ~ "South",
    endsWith(dff.State, "AZ") ~ "West",
    endsWith(dff.State, "CO") ~ "West",
    endsWith(dff.State, "ID") ~ "West",
    endsWith(dff.State, "NM") ~ "West",
    endsWith(dff.State, "MT") ~ "West",
    endsWith(dff.State, "UT") ~ "West",
    endsWith(dff.State, "NV") ~ "West",
    endsWith(dff.State, "WY") ~ "West",
    endsWith(dff.State, "AK") ~ "West",
    endsWith(dff.State, "CA") ~ "West",
    endsWith(dff.State, "HI") ~ "West",
    endsWith(dff.State, "OR") ~ "West",
    endsWith(dff.State, "WA") ~ "West",
  ))


Region_summary <- weighted_data_state %>% tbl_summary(by = Status, statistic = all_continuous() ~ "{mean}")
summary(Region_summary)
midwest_summary <- data.frame(Region_summary$table_body$stat_1)
northeast_summary <- data.frame(Region_summary$table_body$stat_2)
south_summary <- data.frame(Region_summary$table_body$stat_3)
west_summary <- data.frame(Region_summary$table_body$stat_4)
region_table <- data.frame(c('Midwest', 'Northeast', 'South', 'West'))
averages_by_region <- cbind(midwest_summary, northeast_summary, south_summary, west_summary)
slice <- 55 + nrow(summary_subtopic_2)
averages_by_region <- averages_by_region %>% slice(slice)    
regions <- c('Midwest', 'Northeast', 'South', 'West')
regions <- data.frame(regions)
averages_by_region <- transpose(averages_by_region)
averages_by_region <- cbind(regions, averages_by_region) 


# source of code: https://gis.stackexchange.com/questions/403856/how-to-create-a-us-map-in-r-with-separation-between-states-when-using-albersusa
state_ISO2 <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
regions_census_main <- c("South", "West", "West", "South", "West", "West", "Northeast", "South", "South", "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", "South", "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast", "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", "Northeast", "Northeast", "South", "Midwest", "South", "South", "West", "Northeast", "South", "West", "South", "Midwest", "West")
regions_census_main_value <- rep(0, length(regions_census_main))
regions_census_main_value[regions_census_main == "West"] <- averages_by_region[4,2]
regions_census_main_value[regions_census_main == "Midwest"] <- averages_by_region[1,2]
regions_census_main_value[regions_census_main == "Northeast"] <- averages_by_region[2,2]
regions_census_main_value[regions_census_main == "South"] <- averages_by_region[3,2]
regions_census_main_value <- as.numeric(regions_census_main_value)

dataus <- as.data.frame(cbind(state_ISO2, regions_census_main, regions_census_main_value), stringsAsFactors = FALSE)

West <- averages_by_region[4,2]
Northeast <- averages_by_region[2,2]
Midwest <- averages_by_region[1,2]
South <- averages_by_region[3,2]


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = plotly::toRGB('white')
)

region_plot <- plot_geo(dataus, locationmode = 'USA-states') %>%
  add_trace(
    z = regions_census_main_value, locations = ~state_ISO2,
    color = regions_census_main_value, colors = "Blues"
  ) %>%
  colorbar(title = "Average Number of Courses", data(regions_census_main_value), limits = c(15,60)) %>%
  layout(hovermode = FALSE,
         title = 'Average Number of Courses by Region',
         geo = g
  )
region_plot %>% add_annotations(bgcolor = "white", bordercolor = "black", borderwidth = 3, x = c(.25,.261,.55,.55,.85,.825,.62,.62), y = c(.52,.490,.55,.520,.60,.57,.42,.39), text = c("West", West, 
                                                                                                                                                                                        "Midwest", Midwest, "Northeast",Northeast, "South", South), showarrow = c(FALSE, FALSE, FALSE, FALSE,FALSE, FALSE, FALSE, FALSE))

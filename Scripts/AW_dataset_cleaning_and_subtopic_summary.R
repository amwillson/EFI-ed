# Clear environment
rm(list = ls())

# Load libraries
library(dplyr)
library(pivottabler)
library(tidyr)
library(janitor)
library(gt)
library(tidyverse)
library(nnet)
library(ROCR)
#library(MASS)
library(forecast)
library(data.table)
library(RColorBrewer)
library(viridis)
library(readxl)
library(usmap)
library(ggplot2)
library(plyr)
library(stringr)
library(gtsummary)
library(plotly)
library(tidyverse)
library(tigris)
library(sf)
library(basictabler)
library(flextable)

# Load data
Courses_by_school <- read_excel('Courses_by_school.xlsx', sheet = 'Data (Hayden)')

# Clean up data
ef.data <- data.frame(Courses_by_school) #push data to dataframe
ef.data <- dplyr::select(Courses_by_school, -c(URL, Notes)) #remove unnecessary columns of dataset
ef.data <- dplyr::select(ef.data, -last_col()) #remove unnecessary columns of dataset
summary(ef.data)
distinct(ef.data, `Sub-topic`) 
ef.data$`Sub-topic` <- recode(ef.data$`Sub-topic`, #renaming variables so that they are uniform
                              `Basic of Ecology` = "Basics of Ecology",
                              `Basics of coding` = "Basics of Coding",
                              `basics of ecology` = "Basics of Ecology",
                              `Basics of ecology` = "Basics of Ecology",
                              `Basics of forecasting` = "Basics of Forecasting",
                              `basics of statistics` = "Basics of Statistics",
                              `Basics of statistics` = "Basics of Statistics",
                              `data visualization` = "Data Visualization",
                              `Data visualization` = "Data Visualization",
                              `Decision science` = "Decision Science",
                              `ethics` = "Ethics",
                              `Basics of Coding` = "Introduction to Coding",
                              `introduction to coding` = "Introduction to Coding",
                              `Introduction to coding` = "Introduction to Coding",
                              `INtroduction to coding` = "Introduction to Coding",
                              `Introduciton to coding` = "Introduction to Coding",
                              `Introduction to computing` = "Introduction to Coding",
                              `Introduciton to Coding` = "Introduction to Coding",
                              `machine learning` = "Machine Learning",
                              `Machine learning` = "Machine Learning",
                              `Science communication` = "Science Communication",
                              `science communication` = "Science Communication",
                              `Science communication` = "Science Communication",
                              `Science communcation` = "Science Communication",
                              `Science Communcation` = "Science Communication",
                              `statistical models` = "Statistical Models",
                              `statistical Models` = "Statistical Models",
                              `Statistical models` = "Statistical Models",
                              `uncertainty` = "Uncertainty",
                              `workflows & open science` = "Workflows & open science",
                              `Workflows & Open science` = "Workflows & open science",
                              `workflows and open science` = "Workflows & open science",
                              `Workflows and open science` = "Workflows & open science",
                              `Workflows and Open science` = "Workflows & open science",
                              `working with data/data manipulation` = "Working with data/data manipulation")
ef.data$Type <- recode(ef.data$Type, `community college` = "Community College")
ef.data$`Highest degree offered` <- recode(ef.data$`Highest degree offered`, #recoding variable names so all uniform
                                                `Associates degree` = "Associate's",
                                                `Associates degree` = "Associate's",
                                                `Assosciate's` = "Associate's")

# Create pivot table summarizing courses in each sub-topic by institution
#want to consolidate the dataset by 
# need to go through dataset and for each unique college name count number of sub-topics and count of each 
pt <- PivotTable$new() # create pivottable 
pt$addData(ef.data) #populate with ef.data
pt$addColumnDataGroups("Sub-topic") #using Sub-Topic data to populate pivottable
pt$addRowDataGroups("College")
pt$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
pt$renderPivot() #create pivot table 
summary_subtopic <- pt$asDataFrame() #pushing pivottable to dataframe for easier use and analysis later
summary_subtopic

# Create summary statistics
summary_subtopic <- summary_subtopic %>% mutate(across(everything(), .fns = ~replace_na(.,0))) #put zeros in for all blank cells
summary_subtopic <- summary_subtopic %>% clean_names() #rename variable names so easier for later manipulation
row_names_to_remove<-c("Total") #had to remove total row because it was taking mean including total row
summary_subtopic_2 <- summary_subtopic[!(row.names(summary_subtopic) %in% row_names_to_remove),] #push to new dataset
mean_by_subtopic <- summary_subtopic_2 %>% summarise_if(is.numeric, mean) #calculate mean across all columns without total row
sd_by_subtopic <- summary_subtopic_2 %>% summarise_if(is.numeric, sd) #calculate sd across all rows
#Total <- data.frame(summary_subtopic$total)
#Total<-as.data.frame(t(Total))
rownames(mean_by_subtopic) <- ("Mean") #renaming row name
rownames(sd_by_subtopic) <- ("sd") #renaming row name
sd_mean_subtopics <- rbind(summary_subtopic, mean_by_subtopic, sd_by_subtopic) #merge the the datasets/vectors
is.num <- sapply(sd_mean_subtopics, is.numeric) #
sd_mean_subtopics[is.num] <- lapply(sd_mean_subtopics[is.num], round, 2) #rounding all values in dataset to 2 decimals

# Produce tables of mean & sd and sd_mean_subtopics
mean_by_subtopic_tab = as.data.frame(t(mean_by_subtopic))
mean_by_subtopic_tab = cbind(rownames(mean_by_subtopic_tab), mean_by_subtopic_tab)
colnames(mean_by_subtopic_tab) = c('Sub-topic', 'Mean')
mean_by_subtopic_tab = mean_by_subtopic_tab[-19,]
mean_by_subtopic_tab[,1] = c('Basics of Coding', 'Basics of Ecology', 'Basics of Forecasting',
                             'Basics of Statistics', 'Basics of Statistics/Decision Science',
                             'Basics of Statistics/Introduction to Coding', 'Data Visualization',
                             'Data Visualization/Statistical Modeling', 'Decision Science',
                             'Ethics', 'Introduction to Coding', 'Machine Learning', 'Model Assessment',
                             'Science Communication', 'Statistical Models', 'Uncertainty', 'Workflows & Open Science',
                             'Working with Data/Data Manipulation', 'Total')

mean_by_subtopic_tab %>%
  gt() %>%
  fmt_markdown(columns = c('Mean')) %>%
  tab_header(
    title = md('Average number of courses in each sub-topic')) %>%
  fmt_number(columns = 'Mean', decimals = 2) %>%
  gtsave(filename = 'mean_by_subtopic.png')

#### TO DO: Make other 2 tables

#now going to add relevant variables back in like state, school classification, highest degree offered etc.
reduced_data <- dplyr::select(ef.data, -c(Course, Department, `Sub-topic`)) #removing columns from dataset
reduced_data <- distinct(reduced_data, College, .keep_all = TRUE) #this removes all duplicate rows based on college name, so now there should the same number of rows as distinct colleges
reduced_data$`Highest degree offered` <- recode(reduced_data$`Highest degree offered`, #recoding variable names so all uniform
                                     `Associates degree` = "Associate's",
                                     `Associates degree` = "Associate's",
                                     `Assosciate's` = "Associate's")
reduced_data$Type <- recode(reduced_data$Type, `community college` = "Community College") # recoding variable names so all uniform
numeric_type <- data.frame(reduced_data$Type) 
must_convert<-sapply(numeric_type,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(numeric_type[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out<-cbind(numeric_type[,!must_convert],M2)
dff <- merge(data.frame(reduced_data, row.names=NULL), data.frame(sd_mean_subtopics, row.names=NULL), 
             by = 0, all = TRUE)[-1] #merging dataframes to create full dataframe with college data and summarized subtopic data
data_for_regression <- merge(data.frame(reduced_data, row.names=NULL), data.frame(summary_subtopic_2, row.names=NULL), 
                    by = 0, all = TRUE)[-1] #merge another dataset that is easier to use for the regression/BCLM
dff <- dff %>% arrange(College) #arranging rows in alphabetical order by college name 

#Planning out next steps
#Things needed to accomplish by next meeting, visualize basic statistics like mean
#Need to calculate a chisq
#divide data into training and predicting datasets using 

### REGRESSION ###

train.index <- sample(nrow(data_for_regression), nrow(data_for_regression)*.5) #creating training data set for BCLM
train.index
train.df <- data_for_regression[train.index, ]

valid.df <- data_for_regression[-train.index, ] #validation set for BCLM
regress.school.type <- multinom(Type ~ basics_of_ecology + basics_of_forecasting + basics_of_statistics + data_visualization + decision_science + ethics + introduction_to_coding + machine_learning + model_assessment
                           + science_communication + statistical_models + uncertainty + workflows_open_science + working_with_data_data_manipulation, data = train.df) #created Baseline categorical logit model (BCLM)
stepAIC(regress.school.type)
regress.stepAIC <- multinom(formula = Type ~ basics_of_forecasting + data_visualization + 
           ethics + machine_learning + working_with_data_data_manipulation, 
         data = train.df)
regress.school.type 
pred.org <- data.frame(predict(regress.school.type, valid.df)) #now predicted the type of schools in the validation data set
pred.AIC <- data.frame(predict(regress.stepAIC, valid.df))
summary(pred.org); summary(pred.AIC) #summary of the predictions
#need to add a line to see how well the prediction was.... like %
valid.pred <- predict(regress.school.type, valid.df, type = "response")
accuracy(pred, valid.df$Type)
confusionMatrix(as.factor(ifelse(regress.school.type$fitted > 0.5, 1, 0)), as.factor(train.df[,5]))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = T)
abline(a = 1, b = -1)
library(generalhoslem)
logitgof(train.df$Type, regress.school.type$fitted.values, g = 5, ord = F) #should do a general hoslem test to determine if the model is an adequate prediction
mean_by_subtopic <- as.data.frame(t(mean_by_subtopic))

### PLOTS & VISUALIZATIONS ###
# What do we want to visualize?
#   -some plot to visualize total number of courses and how they are divided
#   -would be interesting to do a visualization based on geography 
# grouped bars with different curriculum levels
  #x sub-topics
  #y frequency
  #put states in vectors without hawaii
  #coordinate system in tidyverse

### Chisq ###
row.number<- nrow(summary_subtopic)
N <- row.number-1
Total <- summary_subtopic[-(1:N), , drop = FALSE]
C <- ncol(Total)-1
expected <- Total$total/C
expected.vec <- (rep(expected,C))
#rownames(expected.vec) <- ("Expected")
chisq.table <- rbind(Total, expected.vec)
chisq.table <- dplyr::select(chisq.table, -last_col())
chisq.table <- data.frame(chisq.table)
rownames(chisq.table) <- c("Observed","Expected")
chisq.test(chisq.table)

#descriptive stats
#visualizations
#interpret model results 


### Bar plot ###
colnames <- data.frame(colnames(Total))
Total <- transpose(Total)
Total <- cbind(Total, colnames)
Total <- head(Total, -1)
Total <-Total %>% dplyr::rename('Subtopics'= colnames.Total., 'Number of Courses' = V1)
barplot_subtopic_total <- ggplot(Total, aes(x=reorder(Subtopics,`Number of Courses`), y=`Number of Courses`)) + 
  geom_bar(position="stack", stat = "identity") + 
  coord_flip() + 
  labs(title = "Number of Courses by Subtopic") + 
  xlab("Subtopics") +
  theme_linedraw() +
  scale_x_discrete(labels = c('basics_of_ecology' = 'Basics of Ecology',
                              'basics_of_statistics' = 'Basics of Statistics',
                              'working_with_data_data_manipulation' = 'Working with Data/Data Manipulation',
                              'introduction_to_coding' = 'Introduction to Coding',
                              'machine_learning' = 'Machine Learning',
                              'statistical_models' = 'Statistical Models',
                              'workflows_open_science' = 'Workflows & Open Science',
                              'ethics' = 'Ethics',
                              'science_communication' = 'Science Communication',
                              'uncertainty' = 'Uncertainty',
                              'data_visualization' = 'Data Visualization',
                              'basics_of_forecasting' = 'Basics of Forecasting',
                              'na' = 'NA',
                              'decision_science' = 'Decision Science',
                              'basics_of_statistics_introduction_to_coding' = 'Basics of Statistics/Introduction to Coding',
                              'model_assessment' = 'Model Assessment',
                              'data_visualization_statistical_modeling' = 'Data Visualization/Statistical Modeling',
                              'basics_of_statistics_decision_science' = 'Basics of Statistics/Decision Science',
                              'basics_of_coding' = 'Basics of Coding'))
barplot_subtopic_total



### Pie Charts ###
color_count <- length(Total$Subtopics)
coul <- brewer.pal(4, "Reds")
coul <- colorRampPalette(coul)(color_count)

#(Total$`Number of Courses`, labels = Total$`Number of Courses`, main = "Pie Chart of Course Distribution", col = rainbow(length(Total$`Number of Courses`))) + legend("topright", Total$Subtopics, cex = 0.8, fill = rainbow(length(Total$`Number of Courses`))) #basic pie chart

ggplot_pie_chart <- ggplot(Total, aes(x="", y=`Number of Courses`, fill=Subtopics)) + geom_bar(stat="identity", width=1, color = "black") + coord_polar("y", start=0) + theme_void() + labs(title = "Total Courses by Suptopic")
ggplot_pie_chart + scale_fill_grey(start = 0.1, end = .9)
ggplot_pie_chart + scale_fill_viridis(discrete = TRUE)
ggplot_pie_chart + scale_fill_manual(values = coul)
ggplot_pie_chart  
### pie chart make color changes to make it easier to interpret 


###Map Visualization###
College_Locations <- read_excel('College Location Data.xlsx', sheet  = 'College Location Data')
course_total <- data.frame(summary_subtopic_2$total)
transformed_data <- usmap_transform(College_Locations)
map_locations <- plot_usmap("states") + geom_point(data = transformed_data, aes(x = Longitude.1, y = Latitude.1), color = "red", size = 1)
map_locations
weighted.data <- cbind(transformed_data, course_total)

sort.df <- with(data_for_regression,  data_for_regression[order(College) , ])
Type <- data.frame(sort.df$Type)
type_weighted_data <- cbind(weighted.data, Type)

type_weighted_data <-type_weighted_data %>% dplyr::rename('Total Courses'= summary_subtopic_2.total, Type = sort.df.Type)

map_weighted_data <- plot_usmap("states") + geom_point(data = type_weighted_data, aes(x = Longitude.1, y = Latitude.1, size=`Total Courses`, color = Type))
map_weighted_data
us_scatter_type_courses <- map_weighted_data + theme(legend.position = "left") + labs(title = "Courses by College Type")
us_scatter_type_courses #finalized weighted map

### Map Visualization By Region ###

college_state <- data.frame(dff$State)
college_state <- head(college_state, -3)
weighted_data_state <- cbind(weighted.data, college_state)

levels(factor(type_weighted_data$sort.df.Type))


region_state <- weighted_data_state %>%
  pivot_longer(names_to = "variable", values_to = "value", cols=-1:-2)  %>%
  left_join(Edu.Gend, by="variable" )              %>%
  select(dff.State, Region, College, Longitude.1, Latitude.1, summary_subtopic_2.total)       %>%
  mutate(Level = factor(Level, 
                        levels = c("All","NoHS","HS", "AD", "BD", "Grad"))) %>%
  left_join(st.reg , by="State")

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

Region_summary <- weighted_data_state %>% tbl_summary(by = Status)
Region_summary
midwest_summary <- data.frame(Region_summary$table_body$stat_1)
northeast_summary <- data.frame(Region_summary$table_body$stat_2)
south_summary <- data.frame(Region_summary$table_body$stat_3)
west_summary <- data.frame(Region_summary$table_body$stat_4)
region_table <- data.frame(c('Midwest', 'Northeast', 'South', 'West'))
averages_by_region <- cbind(midwest_summary, northeast_summary, south_summary, west_summary)
averages_by_region <- averages_by_region %>% slice(69)    
regions <- c('Midwest', 'Northeast', 'South', 'West')
regions <- data.frame(regions)
averages_by_region <- transpose(averages_by_region)
averages_by_region <- cbind(regions, averages_by_region) #lines 210-287 summarized average number of courses per college based on region of country

colnames(averages_by_region) = c('Region', 'Mean')

averages_by_region %>%
  gt() %>%
  fmt_markdown(columns = c('Region', 'Mean')) %>%
  tab_header(
    title = md('Average number of courses on ecological forecasting per region')) %>%
  gtsave(filename = 'averages_by_region.png')

#need to create map by 

#region <- map_data("region")


# Download state data and filter states
sts <- states() %>%
  filter(!STUSPS %in% c('HI', 'AK', 'PR', 'GU', 'VI', 'AS', 'MP'))

# Summarize to DIVISION polygons, see sf::st_union
#div <- sts %>%
#  group_by(DIVISION) %>% 
#  summarize()

# Plot it

state_ISO <- c("US-AL", "US-AK", "US-AZ", "US-AR", "US-CA", "US-CO", "US-CT", "US-DE", "US-FL", "US-GA", "US-HI", "US-ID", "US-IL", "US-IN", "US-IA", "US-KS", "US-KY", "US-LA", "US-ME", "US-MD", "US-MA", "US-MI", "US-MN", "US-MS", "US-MO", "US-MT", "US-NE", "US-NV", "US-NH", "US-NJ", "US-NM", "US-NY", "US-NC", "US-ND", "US-OH", "US-OK", "US-OR", "US-PA", "US-RI", "US-SC", "US-SD", "US-TN", "US-TX", "US-UT", "US-VT", "US-VA", "US-WA", "US-WV", "US-WI", "US-WY", "US-DC")
state_ISO2 <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")
state_name <- c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming", "District of Columbia")
state_name_lower <- c("alabama", "alaska", "arizona", "arkansas", "california", "colorado", "connecticut", "delaware", "florida", "georgia", "hawaii", "idaho", "illinois", "indiana", "iowa", "kansas", "kentucky", "louisiana", "maine", "maryland", "massachusetts", "michigan", "minnesota", "mississippi", "missouri", "montana", "nebraska", "nevada", "new hampshire", "new jersey", "new mexico", "new york", "north carolina", "north dakota", "ohio", "oklahoma", "oregon", "pennsylvania", "rhode island", "south carolina", "south dakota", "tennessee", "texas", "utah", "vermont", "virginia", "washington", "west virginia", "wisconsin", "wyoming", "district of columbia")
type <- c("state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "state", "district")
regions_census_main <- c("South", "West", "West", "South", "West", "West", "Northeast", "South", "South", "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", "South", "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast", "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", "Northeast", "Northeast", "South", "Midwest", "South", "South", "West", "Northeast", "South", "West", "South", "Midwest", "West", "South")
regions_census_main_value <- c(21, 39, 39, 21, 39, 39, 31, 21, 21, 21, 39, 39, 26, 26, 26, 26, 21, 21, 31, 21, 31, 26, 26, 21, 26, 39, 26, 39, 31, 31, 39, 31, 21, 26, 26, 21, 39, 31, 31, 21, 26, 21, 21, 39, 31, 21, 39, 21, 26, 39, 21)

dataus <- as.data.frame(cbind(state_ISO, state_ISO2, state_name, state_name_lower, type,
                              regions_census_main, regions_census_main_value), stringsAsFactors = FALSE)


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

plot_geo(dataus, locationmode = 'USA-states') %>%
  add_trace(
    z = regions_census_main_value, locations = ~state_ISO2,
    color = regions_census_main_value, colors = 'Blues'
  ) %>%
  colorbar(title = "Regions", data(regions_census_main_value)) %>%
  layout(
    title = 'United States by Regions',
    geo = g
  )
### add number labels into states, change top color make lighter, add labels

### Tiered Bar Plot###
pt.2 <- PivotTable$new() # create pivottable 
pt.2$addData(ef.data) #populate with ef.data
pt.2$addRowDataGroups("Sub-topic")
pt.2$addRowDataGroups("Type", addTotal = FALSE)
pt.2$defineCalculation(calculationName="TotalCourses", summariseExpression="n()")
pt.2$renderPivot() #creates pivot table to summarize subtopics by institution type
data.table <- pt.2$asBasicTable() #pivot table to basic table so easier to manipulate
 data.table$cells$deleteRow(1) #delete original header
data.table$cells$insertRow(1) #add header row
data.table$cells$setCell(1,1, cellType = "columnHeader", rawValue = "Subtopic")
data.table$cells$setCell(1,2, cellType = "columnHeader", rawValue = "Type")
data.table$cells$setCell(1,3, cellType = "columnHeader", rawValue = "Total")
data.table$cells$deleteRow(data.table$rowCount) #delete total row at end
data.table$renderTable()
data.table <- data.table$asDataFrame(firstRowAsColumnNames = TRUE, firstColumnAsRowNames = FALSE, rawValue = TRUE, stringsAsFactors = NULL) #table to dataframe
data.table <- data.table %>% fill(Subtopic)
#data.table <- arrange(data.table, Total)
tiered_barplot <- ggplot((data.table), aes(fill=Type, y=Total, x=reorder(Subtopic, Total))) + 
  geom_bar(position="stack", stat="identity") + coord_flip() + 
  labs(title = "Course Distribution by Subtopic and Institution Type") + 
  ylab("Total Number of Courses") + xlab("Subtopic") +
  theme_linedraw()

ef.data %>%
  ggplot(aes(x = `Sub-topic`, fill = Type)) +
  geom_bar(stat = 'count', position = 'stack') +
  coord_flip() +
  scale_x_discrete(limits = c('Basics of Coding', 'Basics of statistics/Decision science',
                              'Data visualization/statistical modeling', 'Model assessment',
                              'Basics of statistics/Introduction to coding',
                              'Decision Science', 'NA', 'Basics of Forecasting', 'Data Visualization',
                              'Uncertainty', 'Science Communication', 'Ethics',
                              'Workflows & open science', 'Statistical Models', 'Machine Learning', 
                              'Working with data/data manipulation',
                              'Introduction to Coding', 'Basics of Statistics', 'Basics of Ecology')) +
  theme_linedraw()

### Averages ###

  ### Table: Average Number of Courses by Suptopic by Institution Type
average_courses_by_int_type <- PivotTable$new()
average_courses_by_int_type$addData(ef.data)
average_courses_by_int_type$addColumnDataGroups("Type", addTotal = FALSE)
average_courses_by_int_type$addRowDataGroups("Sub-topic", addTotal = FALSE)
average_courses_by_int_type$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_courses_by_int_type$renderPivot()
average_courses_by_int_type <- average_courses_by_int_type$asDataFrame() #pushing pivottable to dataframe for easier use and analysis la
average_courses_by_int_type <- average_courses_by_int_type %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
Type_count <- data.frame(table(data_for_regression$Type))
average_courses_by_int_type$`2-year vocational/technical school` <- average_courses_by_int_type$`2-year vocational/technical school`/Type_count[1,2]
average_courses_by_int_type$`4-year private` <- average_courses_by_int_type$`4-year private`/Type_count[2,2]
average_courses_by_int_type$`4-year public` <- average_courses_by_int_type$`4-year public`/Type_count[3,2]
average_courses_by_int_type$`Community College` <- average_courses_by_int_type$`Community College`/Type_count[4,2]
average_courses_by_int_type$`for profit college` <- average_courses_by_int_type$`for profit college`/Type_count[5,2]
average_courses_by_int_type <- average_courses_by_int_type %>% mutate(across(where(is.numeric), ~ round(., 2)))


  ### Table: Average Number of Courses by Institution Type

average_course_type <- PivotTable$new()
average_course_type$addData(ef.data)
average_course_type$addColumnDataGroups("Type", addTotal = FALSE)
average_course_type$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_type$renderPivot()
average_course_type <- average_course_type$asDataFrame()
average_course_type <- average_course_type %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
average_course_type$`2-year vocational/technical school` <- average_course_type$`2-year vocational/technical school`/Type_count[1,2]
average_course_type$`4-year private` <- average_course_type$`4-year private`/Type_count[2,2]
average_course_type$`4-year public` <- average_course_type$`4-year public`/Type_count[3,2]
average_course_type$`Community College` <- average_course_type$`Community College`/Type_count[4,2]
average_course_type$`for profit college` <- average_course_type$`for profit college`/Type_count[5,2]
average_course_type <- average_course_type %>% mutate(across(where(is.numeric), ~ round(., 2)))


  ### Table: Average Number of Courses by Highest Degree Offered

average_course_degree <- PivotTable$new()
average_course_degree$addData(ef.data)
average_course_degree$addColumnDataGroups("Highest degree offered", addTotal = FALSE)
average_course_degree$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_degree$renderPivot()
average_course_degree <- average_course_degree$asDataFrame()
average_course_degree <- average_course_degree %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
Degree_count <- data.frame(table(data_for_regression$Highest.degree.offered))
average_course_degree$`Associate's`<-average_course_degree$`Associate's`/Degree_count[1,2]
average_course_degree$`Bachelor's` <- average_course_degree$`Bachelor's`/Degree_count[2,2]
average_course_degree$`Master's` <- average_course_degree$`Master's`/Degree_count[3,2]
average_course_degree$PhD <- average_course_degree$`PhD`/Degree_count[4,2]
average_course_degree$`Professional terminal degree` <- average_course_degree$`Professional terminal degree`/Degree_count[5,2]
average_course_degree <- average_course_degree %>% mutate(across(where(is.numeric), ~ round(., 2)))

  ### Table: Average number of courses by suptopic by highest degree offered

average_course_subtopic_degree <- PivotTable$new()
average_course_subtopic_degree$addData(ef.data)
average_course_subtopic_degree$addColumnDataGroups("Highest degree offered", addTotal = FALSE)
average_course_subtopic_degree$addRowDataGroups("Sub-topic", addTotal = FALSE)
average_course_subtopic_degree$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_subtopic_degree$renderPivot()
average_course_subtopic_degree <- average_course_subtopic_degree$asDataFrame() #pushing pivottable to dataframe for easier use and analysis la
average_course_subtopic_degree <- average_course_subtopic_degree %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
average_course_subtopic_degree$`Associate's`<-average_course_subtopic_degree$`Associate's`/Degree_count[1,2]
average_course_subtopic_degree$`Bachelor's` <- average_course_subtopic_degree$`Bachelor's`/Degree_count[2,2]
average_course_subtopic_degree$`Master's` <- average_course_subtopic_degree$`Master's`/Degree_count[3,2]
average_course_subtopic_degree$PhD <- average_course_subtopic_degree$`PhD`/Degree_count[4,2]
average_course_subtopic_degree$`Professional terminal degree` <- average_course_subtopic_degree$`Professional terminal degree`/Degree_count[5,2]
average_course_subtopic_degree <- average_course_subtopic_degree %>% mutate(across(where(is.numeric), ~ round(., 2)))


  ### Table: Average number of courses by Carnegie Classification
  ###Table by subtopics carnegie 

average_course_carnegie <- PivotTable$new()
average_course_carnegie$addData(ef.data)
average_course_carnegie$addColumnDataGroups("Carnegie classification", addTotal = FALSE)
average_course_carnegie$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_carnegie$renderPivot()
average_course_carnegie <- average_course_carnegie$asDataFrame()
carnegie_count <- data.frame(table(data_for_regression$Carnegie.classification))
average_course_carnegie$`Associate's Colleges: High Career & Technical-High Nontraditional` <- average_course_carnegie$`Associate's Colleges: High Career & Technical-High Nontraditional`/carnegie_count[1,2]
average_course_carnegie$`Associate's Colleges: High Career & Technical-High Traditional` <- average_course_carnegie$`Associate's Colleges: High Career & Technical-High Traditional`/carnegie_count[2,2]
average_course_carnegie$`Associate's Colleges: Mixed Transfer/Career & Technical-High Nontraditional` <- average_course_carnegie$`Associate's Colleges: Mixed Transfer/Career & Technical-High Nontraditional`/carnegie_count[3,2]
average_course_carnegie$`Associate's Colleges: Mixed Transfer/Career & Technical-High Traditional` <- average_course_carnegie$`Associate's Colleges: Mixed Transfer/Career & Technical-High Traditional`/carnegie_count[4,2]
average_course_carnegie$`Baccalaureate Colleges: Arts & Sciences Focus` <- average_course_carnegie$`Baccalaureate Colleges: Arts & Sciences Focus`/carnegie_count[5,2]
average_course_carnegie$`Baccalaureate Colleges: Diverse Fields` <- average_course_carnegie$`Baccalaureate Colleges: Diverse Fields`/carnegie_count[6,2]
average_course_carnegie$`Baccalaureate/Associate's Colleges: Associate's Dominant` <- average_course_carnegie$`Baccalaureate/Associate's Colleges: Associate's Dominant`/carnegie_count[7,2]
average_course_carnegie$`Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's` <- average_course_carnegie$`Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's`/carnegie_count[8,2]
average_course_carnegie$`D/PU` <- average_course_carnegie$`D/PU`/carnegie_count[9,2]
average_course_carnegie$M1 <- average_course_carnegie$M1/carnegie_count[10,2]
average_course_carnegie$M3 <- average_course_carnegie$M3/carnegie_count[11,2]
average_course_carnegie$R1 <- average_course_carnegie$R1/carnegie_count[12,2]
average_course_carnegie$R2 <- average_course_carnegie$R2/carnegie_count[13,2]
average_course_carnegie$`Tribal Colleges` <- average_course_carnegie$`Tribal Colleges`/carnegie_count[14,2]
average_course_carnegie <- average_course_carnegie %>% mutate(across(where(is.numeric), ~ round(., 2)))


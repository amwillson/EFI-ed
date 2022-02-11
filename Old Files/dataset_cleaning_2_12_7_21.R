#install.packages('dplyr')
#install.packages("pivottabler")
#install.packages("tidyr")
#install.packages("janitor")

# Clear environment
rm(list = ls())

# Load libraries
library(dplyr)
library(readxl)
library(pivottabler)
library(tidyr)
library(janitor)
library(tidyverse)
library(nnet)
library(MASS)
library(ROCR)
library(forecast)
library(caret)

#### Load in data ####

# Typically, we start by writing code to load in the data
# You used the import from Excel function, which is the equivalent of
# the following code
Courses_by_school <- read_excel('Courses_by_school.xlsx', sheet = 'Data (Hayden)')

#### Clean up data ####
ef.data <- data.frame(Courses_by_school) #push data to dataframe
# Here you're just printing the dataframe, minus the "College" column
dplyr::select(ef.data, -College)
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
                              `introduction to coding` = "Introduciton to Coding",
                              `Introduction to coding` = "Introduciton to Coding",
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

#### Summarize data ####

# want to consolidate the dataset by 
# need to go through dataset and for each unique college name count number of sub-topics and count of each 

# Pivot table summarizing the number of courses in each sub-topic for each institution
pt <- PivotTable$new() # create pivottable 
pt$addData(ef.data) #populate with ef.data
pt$addColumnDataGroups("Sub-topic") #using Sub-Topic data to populate pivottable
pt$addRowDataGroups("College") #Colleges become rows
pt$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
pt$renderPivot() #create pivot table 
summary_subtopic <- pt$asDataFrame() #pushing pivottable to dataframe for easier use and analysis later
summary_subtopic

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

#### Data prep for statistcs ####

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

#### REGRESSION ####

# Here, you could make the total number and sample size consistent regardless data
# length by replacing 31 with nrow(data_for_regression)
# I would also recommend setting seed so we can compare at least for now
set.seed(10)
train.index <- sample(31, 31*.5) #creating training data set for BCLM
train.index
train.df <- data_for_regression[train.index, ]
valid.df <- data_for_regression[-train.index, ] #validation set for BCLM

# I think it would be good to describe what's happening 
# here a little bit like we talked about
# A good place to start is by reading the documentation of the functions 
# so you know what the default settings are
regress.school.type <- multinom(Type ~ basics_of_ecology + basics_of_forecasting + basics_of_statistics + data_visualization + decision_science + ethics + introduction_to_coding + machine_learning + model_assessment
                           + science_communication + statistical_models + uncertainty + workflows_open_science + working_with_data_data_manipulation, data = train.df) #created Baseline categorical logit model (BCLM)
stepAIC(regress.school.type)

#### Final model ####

# Remember that interpretation is really important here
# You don't have to write your interpretation in the code but just a reminder
regress.stepAIC <- multinom(formula = Type ~ basics_of_forecasting + data_visualization + 
           ethics + machine_learning + working_with_data_data_manipulation, 
         data = train.df)
regress.school.type 
pred.org <- data.frame(predict(regress.school.type, valid.df)) #now predicted the type of schools in the validation data set
pred.AIC <- data.frame(predict(regress.stepAIC, valid.df))
summary(pred.org); summary(pred.AIC) #summary of the predictions

# need to add a line to see how well the prediction was.... like %
valid.pred <- predict(regress.school.type, valid.df, type = "response")
accuracy(pred, valid.df$Type)
confusionMatrix(as.factor(ifelse(regress.school.type$fitted > 0.5, 1, 0)), as.factor(train.df[,5]))
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
plot(perf, colorize = T)
abline(a = 1, b = -1)
#library(generalhoslem)
logitgof(train.df$Type, regress.school.type$fitted.values, g = 5, ord = F) #should do a general hoslem test to determine if the model is an adequate prediction
mean_by_subtopic <- as.data.frame(t(mean_by_subtopic))
barplot(mean_by_subtopic, main = Mean)


### PLOTS & VISUALIZATIONS ###
# What do we want to visualize?
#   -some plot to visualize total number of courses and how they are divided
#   -would be interesting to do a visualization based on geography 
# grouped bars with different curriculum levels
  #x sub-topics
  #y frequency
  #put states in vectors without hawaii
  #coordinate system in tidyverse



bp<- ggplot(df, aes(x="", y=value, fill=))+
  geom_bar(width = 1, stat = "identity")
bp

### Chisq ###
row.number<- nrow(summary_subtopic)
N <- row.number-1
Total <- summary_subtopic[-(1:N), , drop = FALSE]
C <- ncol(Total)-1
expected <- Total$total/C
expected.vec <- (rep(expected,C))
rownames(expected.vec) <- ("Expected")
chisq.table <- rbind(Total, expected.vec)
chisq.table <- select(chisq.table, -last_col())
chisq.table <- data.frame(chisq.table)
rownames(chisq.table) <- c("Observed","Expected")
chisq.test(chisq.table)

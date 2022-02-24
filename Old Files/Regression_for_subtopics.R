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

# Clear environment
rm(list = ls())
# Courses_by_school <- read_excel('Courses_by_school_final.xlsx', sheet = 'Combined_Final')
# 
# ef.data <- data.frame(Courses_by_school) #push data to dataframe
# ef.data <- dplyr::select(Courses_by_school, -c(URL, Notes)) #remove unnecessary columns of dataset
# ef.data <- dplyr::select(ef.data, -last_col()) #remove unnecessary columns of dataset
# summary(ef.data)
# distinct(ef.data, `Sub-topic`) 
# ef.data$`Sub-topic` <- recode(ef.data$`Sub-topic`, #renaming variables so that they are uniform
#                               `Basic of Ecology` = "Basics of Ecology",
#                               `Basics of coding` = "Basics of Coding",
#                               `basics of ecology` = "Basics of Ecology",
#                               `Basics of ecology` = "Basics of Ecology",
#                               `Basics of forecasting` = "Basics of Forecasting",
#                               `basics of statistics` = "Basics of Statistics",
#                               `Basics of statistics` = "Basics of Statistics",
#                               `data visualization` = "Data Visualization",
#                               `Data visualization` = "Data Visualization",
#                               `Data Manipulation/processing` = 'Data manipulation/processing',
#                               `Data sources` = "Data Sources",
#                               `Decision science` = "Decision Science",
#                               `ethics` = "Ethics",
#                               `Basics of Coding` = "Introduction to Coding",
#                               `introduction to coding` = "Introduction to Coding",
#                               `Introduction to coding` = "Introduction to Coding",
#                               `INtroduction to coding` = "Introduction to Coding",
#                               `Introduciton to coding` = "Introduction to Coding",
#                               `Introduction to computing` = "Introduction to Coding",
#                               `Introduciton to Coding` = "Introduction to Coding",
#                               `Introduction to programming` = "Introduction to Coding",
#                               `machine learning` = "Machine Learning",
#                               `Machine learning` = "Machine Learning",
#                               `Science communication` = "Science Communication",
#                               `science communication` = "Science Communication",
#                               `Science communication` = "Science Communication",
#                               `Science communcation` = "Science Communication",
#                               `Science Communcation` = "Science Communication",
#                               `statistical models` = "Statistical Models",
#                               `statistical Models` = "Statistical Models",
#                               `Statistical models` = "Statistical Models",
#                               `uncertainty` = "Uncertainty",
#                               `workflows & open science` = "Workflows & open science",
#                               `Workflows & Open science` = "Workflows & open science",
#                               `workflows and open science` = "Workflows & open science",
#                               `Workflows and open science` = "Workflows & open science",
#                               `Workflows and Open science` = "Workflows & open science",
#                               `working with data/data manipulation` = "Working with data/data manipulation",
#                               `Working with Data` = "Working with data",
#                               `Working wtih data` = "Working with data")
# ef.data$Type <- recode(ef.data$Type, `community college` = "Community College")
# ef.data$`Highest degree offered` <- recode(ef.data$`Highest degree offered`, #recoding variable names so all uniform
#                                                 `Associates degree` = "AA/AS",
#                                                 `Associates degree` = "AA/AS",
#                                                 `Assosciate's` = "AA/AS")
#want to consolidate the dataset by 
# need to go through dataset and for each unique college name count number of sub-topics and count of each 
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

#summary_subtopic <- summary_subtopic %>% clean_names() #rename variable names so easier for later manipulation
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
#now going to add relevant variables back in like state, school classification, highest degree offered etc.
reduced_data <- dplyr::select(ef.data, -c(`Sub.topic`)) #removing columns from dataset
reduced_data <- distinct(reduced_data, College, .keep_all = TRUE) #this removes all duplicate rows based on college name, so now there should the same number of rows as distinct colleges
# reduced_data$`Highest degree offered` <- recode(reduced_data$`Highest degree offered`, #recoding variable names so all uniform
#                                      `Associates degree` = "AA/AS",
#                                      `Associates degree` = "AA/AS",
#                                      `Assosciate's` = "AA/AS")
#reduced_data$Type <- recode(reduced_data$Type, `community college` = "Community College") # recoding variable names so all uniform
numeric_type <- data.frame(reduced_data$Type) 
must_convert<-sapply(numeric_type,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(numeric_type[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
out<-cbind(numeric_type[,!must_convert],M2)
dff <- merge(data.frame(reduced_data, row.names=NULL), data.frame(sd_mean_subtopics, row.names=NULL), 
             by = 0, all = TRUE)[-1] #merging dataframes to create full dataframe with college data and summarized subtopic data
data_for_regression <- merge(data.frame(reduced_data, row.names=NULL), data.frame(summary_subtopic_2, row.names=NULL), 
                             by = 0, all = TRUE)[-1] #merge another dataset that is easier to use for the regression/BCLM


### REGRESSION ###

set.seed(8)
train.index <- sample(nrow(data_for_regression), nrow(data_for_regression)*.5) #creating training data set for BCLM
train.index
train.df <- data_for_regression[train.index, ]
valid.df <- data_for_regression[-train.index, ] #validation set for BCLM
table(valid.df$Type)
table(train.df$Type)

colnames(data_for_regression)

regress.school.type <- multinom(Type ~ Basics.of.Coding + Basics.of.Ecology + Basics.of.Forecasting + Basics.of.Statistics + Data.Manipulation
                                + Data.Sources + Data.Visualization + Decision.Science + Ethics + Machine.Learning + Mechanistic.Models + Model.Assessment + 
                                  Probability...Uncertainty + Science.Communication + Statistical.Models + Traditional.Ecological.Knowledge + Workflows...Open.Science + 
                                  Working.with.Data, data = train.df) #created Baseline categorical logit model (BCLM)
#regress.school.type <- multinom(Type ~ basics_of_ecology + basics_of_forecasting + basics_of_statistics + data_manipulation_processing + data_sources + 
                               #   data_visualization + decision_science + ethics + introduction_to_coding + machine_learning + mechanistic_models + model_assessment
                               # + probability_uncertainty + science_communication + statistical_models + traditional_ecological_knowledge + workflows_open_science + working_with_data, data=train.df)
MASS::stepAIC(regress.school.type)
# regress.stepAIC <- multinom(formula = Type ~ Basics.of.Forecasting + Basics.of.Statistics + 
#                               Ethics, data = train.df) #with for profit but smaller df
# regress.stepAIC <- multinom(formula = Type ~ basics_of_ecology + ethics + introduction_to_coding + 
#                             workflows_open_science + working_with_data_data_manipulation, 
#                          data = train.df) #without for profit but smaller df

regress.stepAIC <- multinom(formula = Type ~ Basics.of.Coding + Basics.of.Ecology +
                              Basics.of.Statistics + Data.Sources + Data.Visualization +
                              Ethics + Machine.Learning + Probability...Uncertainty + Working.with.Data,
                            data = train.df)


# stepAIC starts with full modeling, then removes variables one at a time to determine model w/ k-1 predictors that has the lowest AIC
# the best fitting, simplest model will have the lowest AIC
valid.pred.AIC <- predict(regress.stepAIC, valid.df)
train.pred.AIC <- predict(regress.stepAIC, train.df)
valid.pred <- predict(regress.school.type, valid.df)
train.pred <- predict(regress.school.type, train.df)
summary(valid.pred.AIC); summary(train.pred.AIC); summary(valid.pred); summary(train.pred)

#table(valid.df$Type); summary(valid.pred)
#tab <- table(valid.df$Type, valid.pred.AIC)
#tab.2 <- table(valid.df$Type, valid.pred)

train.df$Type <- as.factor(train.df$Type)
valid.df$Type <- as.factor(valid.df$Type)
#round((sum(diag(tab.2))/sum(tab.2))*100,2)
#confusion matrices to compare predicted results to actual Type of institution
confusionMatrix(train.pred.AIC, train.df$Type)
confusionMatrix(train.pred, train.df$Type)
confusionMatrix(valid.pred.AIC, valid.df$Type)
confusionMatrix(valid.pred, valid.df$Type)

summary(train.pred.AIC)
summary(valid.pred)
table(valid.df$Type)

#need to add a line to see how well the prediction was.... like %

logitgof(valid.df$Type, regress.stepAIC$fitted.values, g = 4, ord = F) #should do a general hoslem test to determine if the model is an adequate prediction


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

### Chisq ###
row.number<- nrow(summary_subtopic)
N <- row.number-1
Total <- summary_subtopic[-(1:N), , drop = FALSE]
#C <- ncol(Total)-1
#expected <- Total$Total/C
#expected.vec <- (rep(expected,C))
#chisq.table <- rbind(Total, expected.vec)
#chisq.table <- dplyr::select(chisq.table, -last_col())
#chisq.table <- data.frame(chisq.table)
#rownames(chisq.table) <- c("Observed","Expected")
#chisq <- chisq.test(chisq.table) not correct
Total <- dplyr::select(Total, -last_col())
sq <- chisq.test(Total) #correct chisq
sq


### EF Courses Subtopics ###

load("cleaned_data_EF_course.Rdata")
EF_courses <- data
EF_courses <- data.frame(EF_courses)
unique(EF_courses$Sub.topic)
unique(Total)
EF_courses_subtopics <- PivotTable$new() # create pivottable 
EF_courses_subtopics$addData(EF_courses) #populate with ef.data
EF_courses_subtopics$addColumnDataGroups("Sub.topic", addTotal = FALSE) #using Sub-Topic data to populate pivottable
EF_courses_subtopics$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
EF_courses_subtopics$renderPivot() #create pivot table
EF_courses_subtopics <- EF_courses_subtopics$asDataFrame()



###Chisq Total Courses vs EF_Courses (null)###
chisq_EF_courses_total_courses <- bind_rows(EF_courses_subtopics, Total)
#chisq_all_courses_online_resources <- dplyr::select(chisq_all_courses_online_resources, -last_col())
chisq_EF_courses_total_courses <- chisq_EF_courses_total_courses %>% mutate(across(everything(), .fns = ~replace_na(.,0))) #put zeros in for all blank cells
#((chisq_EF_courses_total_courses[2,]-chisq_EF_courses_total_courses[1,])**2)/chisq_EF_courses_total_courses[1,]
courses_vs_ef_courses <- fisher.test(chisq_EF_courses_total_courses, simulate.p.value = TRUE, B = 10000)
courses_vs_ef_courses$p.value

### Online Resources vs EF_Courses (null)###

load("cleaned_data_online.Rdata")
online_resources <- data.frame(data)
EF_online_subtopics <- PivotTable$new() # create pivottable 
EF_online_subtopics$addData(online_resources) #populate with ef.data
EF_online_subtopics$addColumnDataGroups("Category", addTotal = FALSE) #using Sub-Topic data to populate pivottable
EF_online_subtopics$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
EF_online_subtopics$renderPivot() #create pivot table
EF_online_subtopics <- EF_online_subtopics$asDataFrame()
colnames(EF_online_subtopics)
chisq_EF_courses_online_resources <- bind_rows(EF_courses_subtopics, EF_online_subtopics)
chisq_EF_courses_online_resources <- chisq_EF_courses_online_resources %>% mutate(across(everything(), .fns = ~replace_na(.,0))) #put zeros in for all blank cells
online_vs_ef_courses <- fisher.test(chisq_EF_courses_online_resources, simulate.p.value = TRUE, B = 10000)
online_vs_ef_courses$p.value


total_online <- bind_rows(Total, EF_online_subtopics)
total_online <- total_online %>% mutate(across(everything(), .fns = ~replace_na(.,0))) #put zeros in for all blank cells
fisher.test(total_online, simulate.p.value = TRUE, B = 10000)

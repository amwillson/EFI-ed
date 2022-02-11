## This script cleans the data collected on ecological forecasting courses

## Author: AM Willson & H Gallo
## Date modified: 10 February 2022

rm(list = ls())

library(tidyverse)

data = read.csv('Data/EF_courses.csv')

# Remove columns that are unnecessary after processing data
data = data %>%
  select(-Syllabus.location, -Course.Name, -Instructor.s.)

# Look at unique entries in relevant columns
unique(data$Institution)
unique(data$Education.Level)
unique(data$Carnegie.Classification)
unique(data$Sub.topic)

# Make simpler Carnegie classification
data = data %>%
  mutate(Carnegie.Classification.2 = recode(Carnegie.Classification,
                                            'Baccalaureate Colleges: Arts & Sciences Focus' = 'B'))

# Correct error in sub-topic
data = data %>%
  mutate(Sub.topic = recode(Sub.topic,
                            'Data Manipulation/Processing' = 'Data Manipulation',
                            'Uncertainty' = 'Probability & Uncertainty'))

save(data, file = 'Data/cleaned_data_EF_course.RData')

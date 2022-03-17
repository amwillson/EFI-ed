## This script cleans the raw data on courses related to ecological forecasting

## Author: AM Willson & H Gallo
## Date modified: 17 March 2022

rm(list = ls())

# Load libraries
library(tidyverse)

# Read in data
data = read.csv('Data/Courses_by_school - Combined_Final.csv')

# View unique entries of each column--check for duplicates/inconsistencies
unique(data$College) # 48 schools
unique(data$State) # 27 states
unique(data$Type) # 5 instituion types
unique(data$Highest.degree.offered) # 5 highest degree types
unique(data$Carnegie.classification) # 19 Carnegie Classifications
unique(data$Sub.topic) # 18 sub-topics

# Fix institution type classification
data = data %>%
  mutate(Type = recode(Type, 
                       '4-year private' = '4-Year Private',
                       '4-year private ' = '4-Year Private',
                       '2-year vocational/technical school' = '2-Year Technical',
                       '2-year vocational/technical' = '2-Year Technical',
                       'community college' = 'Community College',
                       '4-year public' = '4-Year Public',
                       '4-year public ' = '4-Year Public',
                       'for profit college' = 'For Profit'))

# Fix highest degree offered classification
data = data %>%
  mutate(Highest.degree.offered = recode(Highest.degree.offered,
                                         "PhD" = "PhD",
                                         "Associate's" = "AA/AS",
                                         "Assosciate's" = "AA/AS",
                                         "Associates degree" = "AA/AS",
                                         "Master's" = "MA/MS",
                                         "Bachelor's" = "BA/BS",
                                         "Associate's " = "AA/AS",
                                         "Professional terminal degree" = "Other Terminal Degree"))

# Make simpler Carnegie Classification
data = data %>%
  mutate(Carnegie.classification.2 = recode(Carnegie.classification,
                                            "M1" = "M1",
                                            "Associate's Colleges: High Career & Technical-High Nontraditional" = "A",
                                            "Associate's Colleges: Mixed Transfer/Career & Technical-High Nontraditional" = "A",
                                            "R2" = "R2",
                                            "Associate's Colleges: Mixed Transfer/Career & Technical-High Traditional" = "A",
                                            "Baccalaureate Colleges: Diverse Fields" = "B",
                                            "Associate's Colleges: High Career & Technical-High Traditional" = "A",
                                            "M3" = "M3",
                                            "Baccalaureate/Associate's Colleges: Associate's Dominant" = "A/B",
                                            "D/PU" = "D/PU",
                                            "Baccalaureate/Associate's Colleges: Mixed Baccalaureate/Associate's" = "A/B",
                                            "Baccalaureate Colleges: Arts & Sciences Focus" = "B",
                                            "Tribal Colleges" = "TC",
                                            "R1" = "R1",
                                            "Associate's Colleges: High Transfer-Mixed Traditional/Nontraditional" = "A",
                                            "Associate's Colleges: High Transfer-High Traditional" = "A",
                                            "Associate's Colleges: Mixed Transfer/Career & Technical-Mixed Traditional/Nontraditional" = "A",
                                            "Associate's Colleges: High Transfer-High Nontraditional" = "A",
                                            "Associate's - Public Urban-serving Single Campus" = "A"))

# Fix sub-topic categories
data = data %>%
  mutate(Sub.topic = recode(Sub.topic,
                            "Basics of Statistics" = "Basics of Statistics",
                            "Working with data" = "Working with Data",
                            "Data Visualization" = "Data Visualization",
                            "Statistical models" = "Statistical Models",
                            "Data manipulation/processing" = "Data Manipulation",
                            "Basics of Ecology" = "Basics of Ecology",
                            "Basics of statistics" = "Basics of Statistics",
                            "Science communication" = "Science Communication",
                            "Basics of ecology" = "Basics of Ecology",
                            "Decision science" = "Social Science",
                            "Data visualization" = "Data Visualization",
                            "Machine Learning" = "Machine Learning",
                            "Introduction to coding" = "Basics of Coding",
                            "Basics of forecasting" = "Basics of Forecasting",
                            "Ethics" = "Ethics",
                            "Probability & Uncertainty" = "Probability & Uncertainty",
                            "introduction to coding" = "Basics of Coding",
                            "Workflows and open science" = "Workflows & Open Science",
                            "Data sources" = "Data Sources",
                            "Basics of Coding" = "Basics of Coding",
                            "Machine learning" = "Machine Learning",
                            "Basics of coding" = "Basics of Coding",
                            "Mechanistic models" = "Mechanistic Models",
                            "basics of statistics" = "Basics of Statistics",
                            "Science Communication" = "Science Communication",
                            "Data Sources" = "Data Sources",
                            "Data visualization " = "Data Visualization",
                            "Decision Science" = "Social Science",
                            "science communication" = "Science Communication",
                            "data visualization" = "Data Visualization",
                            "Science Communication " = "Science Communication",
                            "Statistical Models" = "Statistical Models",
                            "Science communication " = "Science Communication",
                            "machine learning" = "Machine Learning",
                            "statistical Models" = "Statistical Models",
                            "basics of ecology" = "Basics of Ecology",
                            "science communication " = "Science Communication",
                            "Basics of Forecasting" = "Basics of Forecasting",
                            "statistical models" = "Statistical Models",
                            "Introduction to coding " = "Basics of Coding",
                            "Science Communcation" = "Science Communication",
                            "Science communcation" = "Science Communication",
                            "Model Assessment" = "Model Assessment",
                            "ethics" = "Ethics",
                            "Introduction to computing" = "Basics of Coding",
                            "Basics of statistics " = "Basics of Statistics",
                            "Working with Data" = "Working with Data",
                            "Introduction to programming" = "Basics of Coding",
                            "Basic of Ecology" = "Basics of Ecology",
                            "Introduction to Coding" = "Basics of Coding",
                            "Working wtih data" = "Working with Data",
                            "Data Manipulation/processing" = "Data Manipulation",
                            "Workflows & Open science " = "Workflows & Open Science",
                            "Basics of Ecology " = "Basics of Ecology",
                            "INtroduction to coding" = "Basics of Coding",
                            "Workflows & open science" = "Workflows & Open Science",
                            "Traditional Ecological Knowledge" = "Traditional Ecological Knowledge"))

# Remove unnecessary columns
# These columns were used during data collection but are not relevant to the analysis
data = data %>%
  select(-c(URL, Course, Department, Notes, X))

# Change data types
data$College = as.factor(data$College)
data$State = as.factor(data$State)
data$Type = as.factor(data$Type)
data$Highest.degree.offered = as.factor(data$Highest.degree.offered)
data$Carnegie.classification = as.factor(data$Carnegie.classification)
data$Sub.topic = as.factor(data$Sub.topic)
data$Carnegie.classification.2 = as.factor(data$Carnegie.classification.2)

save(data, file = 'Data/cleaned_data.RData')

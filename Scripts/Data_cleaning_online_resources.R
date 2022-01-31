## This script cleans the QA/QC'd data on online resources related to ecological forecasting
## Prior to importing, author AMW checked all resource submissions via our Google Form and
## categorized the resources according to resource type, topic in forecasting, and resource level

## Author: AM Willson
## Date modified: 31 January 2022

rm(list = ls())

library(tidyverse)

data = read.csv('Data/online_resources_raw.csv')

# Remove columns that are unnecessary after processing data
data = data %>%
  select(-Link, -Author, -Discipline, -Resource.type.1)

# Look at unique entries in relevant columns
unique(data$Resource.type)
unique(data$Category)
unique(data$Education.level)
unique(data$Resource.level)

# Fix resource type column
# Author AW chose to keep only one resource type per entry
# Repository always supersedes other types because this combination means a repository of a given other type
# Course material supersedes video because vieos are used in the classroom setting with this combination
# Module supersedes course material because the course material involves a series of hands-on activities
# Other changes were made on a case-by-case basis
# Some changes were purely to the naming convention

data = data %>%
  mutate(Resource.type = recode(Resource.type,
                                'Course Material' = 'Course Material',
                                'Video' = 'Video',
                                'Repository' = 'External Repository',
                                'Module' = 'Module',
                                'Repository; Module' = 'External Repository',
                                'Module; Course Material to come' = 'Module',
                                'Course Material; Video' = 'Course Material',
                                'Couse Material; Video' = 'Course Material',
                                'Video; Workshop; Repository' = 'External Repository',
                                'Video; Module' = 'Module',
                                'Module; Workshop' = 'Module',
                                'Textbook' = 'Textbook',
                                'Workshop' = 'Workshop',
                                'Course Material; Video' = 'Course Material',
                                'Article' = 'Article',
                                'NetLogo lab' = 'NetLogo Lab',
                                'Course materials (asynchronous or past course)' = 'Course Material',
                                'Course materials (asyncrhonous or past course)' = 'Course Material',
                                'Module (hands-on), Textbook' = 'Module',
                                'Module (hands-on)' = 'Module',
                                'Video series' = 'Video',
                                'Module (hands-on), Course materials (asynchronous or past course)' = 'Module',
                                'Module (hands-on), Course materials (asyncrhonous or past course)' = 'Module',
                                'Repository, Tutorials' = 'External Repository',
                                'Repository, presentation slides' = 'External Repository',
                                'Module (hands-on), Repository' = 'External Repository',
                                'Video series, Course materials (asynchronous or past course)' = 'Course Material',
                                'Video series, Course materials (asyncrhonous or past course)' = 'Course Material',
                                'Video series, Repository' = 'External Repository'))

# Fix category column
# Basics of R and Basics of Python are combined to be consistent with course data
# Uncertainty was changed to Probability & Uncertainty to be consistent with course data
# Other changes were purely cosmetic

data = data %>%
  mutate(Category = recode(Category,
                           'Basics of forecasting' = 'Basics of Forecasting',
                           'Basics of R' = 'Basics of Coding',
                           'Basics of Python' = 'Basics of Coding',
                           'Basics of ecology' = 'Basics of Ecology',
                           'Basics of Statistics' = 'Basics of Statistics',
                           'Data Sources' = 'Data Sources',
                           'Working with Data' = 'Working with Data',
                           'Data' = 'Data Manipulation',
                           'Data visualization tools' = 'Data Visualization',
                           'Statistical models' = 'Statistical Models',
                           'Mechanistic models' = 'Mechanistic Models',
                           'State space models' = 'State Space Models',
                           'Uncertainty' = 'Probability & Uncertainty',
                           'Data assimilation' = 'Data Assimilation',
                           'Iteration' = 'Iteration',
                           'Machine learning' = 'Machine Learning',
                           'Workflows & Open Science' = 'Workflows & Open Science',
                           'Model assessment' = 'Model Assessment',
                           'Decision science' = 'Decision Science',
                           'Interpreting Forecasts' = 'Science Communication',
                           'Forecasting Textbooks' = 'Forecasting Textbooks',
                           'Resource Repositories' = 'Resource Repositories',
                           'Basics of statistics' = 'Basics of Statistics',
                           'Data (manipulation, processing)' = 'Data Manipulation',
                           'Basics of Ecology' = 'Basics of Ecology',
                           'Statistical Models' = 'Statistical Models',
                           'Traditional Ecological Knowledge' = 'Traditional Ecological Knowledge',
                           'Basics of Forecasting' = 'Basics of Forecasting',
                           'Data sources' = 'Data Sources',
                           'machine learning' = 'Machine Learning'))

# Fix education level column
# Only cosmetic changes
data = data %>%
  mutate(Education.level = recode(Education.level,
                                 'Graduate' = 'Graduate',
                                 'Graduate; Professional' = 'Graduate/Professional',
                                 'Undergraduate' = 'Undergraduate',
                                 'Undergraduate; Graduate' = 'Undergraduate/Graduate',
                                 'Graduate; Undergraduate' = 'Undergraduate/Graduate',
                                 'Graduate; Post-Grad' = 'Graduate/Professional',
                                 'Undergraduate; Graduate; Professional' = 'Undergraduate/Graduate/Professional',
                                 'Professional' = 'Professional',
                                 'K-12' = 'K-12',
                                 'Undergraduate level' = 'Undergraduate',
                                 'Graduate level, Undergraduate level' = 'Undergraduate/Graduate',
                                 'Graduate level' = 'Graduate'))

# Fix resource level column
# Only cosmetic changes
data = data %>%
  mutate(Resource.level = recode(Resource.level,
                                 'Advanced' = 'Advanced',
                                 'Introductory' = 'Introductory',
                                 'Introductory; Advanced' = 'Intermediate',
                                 'Introducotry' = 'Introductory',
                                 'Introductory, Advanced' = 'Intermediate'))

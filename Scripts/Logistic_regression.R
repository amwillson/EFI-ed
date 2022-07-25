rm(list = ls())

load('Data/cleaned_data.RData')

data = data %>%
  mutate(Group = if_else(Carnegie.classification.2 
                         %in% c('A', 'A/B', 'B', 'TC'), 
                         'A/B', 'M/D'))

data$Group = as.factor(data$Group)

data$Group = relevel(data$Group, ref = 1)

data = data %>%
  filter(Sub.topic %in% c('Basics of Statistics',
                          'Working with Data',
                          'Data Visualization',
                          'Statistical Models',
                          'Data Manipulation',
                          'Basics of Ecology',
                          'Science Communication',
                          'Social Science',
                          'Machine Learning',
                          'Basics of Coding',
                          'Basics of Forecasting',
                          'Ethics',
                          'Probability & Uncertainty',
                          'Workflows & Open Science',
                          'Data Sources',
                          'Mechanistic Models'))

int_mod = glm(Group ~ 1, family = binomial(link = 'logit'), data = data)
summary(int_mod)

full_mod = glm(Group ~ Sub.topic, family = binomial(link = 'logit'), data = data)
summary(full_mod)

rm(list = ls())

library(tidyverse)
library(RColorBrewer)

data = read.csv('~/Google Drive 2/EFI Education/Existing_resources_110122.csv')
data = data[,1:9]

for(i in 1:nrow(data)){
  if(data$Category[i] %in% c('Basics of ecology', 'Basics of Ecology')){
    data$Category[i] = 'Basics of Ecology'
  }
  if(data$Category[i] %in% c('Basics of forecasting', 'Basics of Forecasting')){
    data$Category[i] = 'Basics of Forecasting'
  }
  if(data$Category[i] %in% c('Basics of Python', 'Basics of R')){
    data$Category[i] = 'Basics of Coding'
  }
  if(data$Category[i] %in% c('Basics of statistics', 'Basics of Statistics')){
    data$Category[i] = 'Basics of Statistics'
  }
  if(data$Category[i] %in% c('Data', 'Data (manipulation, processing)')){
    data$Category[i] = 'Data Manipulation'
  }
  if(data$Category[i] %in% c('Data assimilation')){
    data$Category[i] = 'Data Assimilation'
  }
  if(data$Category[i] %in% c('Data sources', 'Data Sources', 'Working with Data')){
    data$Category[i] = 'Data Sources'
  }
  if(data$Category[i] %in% c('Data visualization tools')){
    data$Category[i] = 'Data Visualization'
  }
  if(data$Category[i] %in% c('Decision science')){
    data$Category[i] = 'Decision Science'
  }
  if(data$Category[i] %in% c('Forecasting Textbooks')){
    data$Category[i] = 'Forecasting Textbooks'
  }
  if(data$Category[i] %in% c('Interpreting Forecasts')){
    data$Category[i] = 'Interpreting Forecasts'
  }
  if(data$Category[i] %in% c('Iteration')){
    data$Category[i] = 'Iteration'
  }
  if(data$Category[i] %in% c('machine learning', 'Machine learning')){
    data$Category[i] = 'Machine Learning'
  }
  if(data$Category[i] %in% c('Mechanistic models')){
    data$Category[i] = 'Mechanistic Models'
  }
  if(data$Category[i] %in% c('Model assessment')){
    data$Category[i] = 'Model Assessment'
  }
  if(data$Category[i] %in% c('Resource Repositories')){
    data$Category[i] = 'Resource Repositories'
  }
  if(data$Category[i] %in% c('State space models')){
    data$Category[i] = 'State Space Models'
  }
  if(data$Category[i] %in% c('Statistical models', 'Statistical Models')){
    data$Category[i] = 'Statistical Models'
  }
  if(data$Category[i] %in% c('Traditional Ecological Knowledge')){
    data$Category[i] = 'Traditional Ecological Knowledge'
  }
  if(data$Category[i] %in% c('Uncertainty')){
    data$Category[i] = 'Uncertainty'
  }
  if(data$Category[i] %in% c('Workflows & Open Science')){
    data$Category[i] = 'Workflows & Open Science'
  }
}

summ = matrix(NA, length(unique(data$Category)), 2)

for(i in 1:length(unique(data$Category))){
  summ[i,1] = unique(data$Category)[i]
  summ[i,2] = length(which(data$Category == unique(data$Category)[i]))
}
colnames(summ) = c('Category', 'n')
summ = as.data.frame(summ)
summ$n = as.numeric(summ$n)

color_count <- length(unique(data$Category))
coul <- colorRampPalette(brewer.pal(8, "Set1"))(color_count)

ggplot_pie_chart = summ %>%
  ggplot(aes(x = '', y = n, fill = Category)) + 
  geom_bar(stat = "identity", width = 1, color = "black", alpha = 0.8) + 
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual(values = coul)
ggplot_pie_chart  

ggsave(ggplot_pie_chart, filename = 'Online_resources.png')

ggplot_pie_chart_2 = summ %>%
  ggplot(aes(x = '', y = n, fill = Category)) + 
  geom_bar(stat = "identity", width = 1, alpha = 0.8) + 
  coord_polar("y", start=0) + 
  theme_void() +
  scale_fill_manual(values = coul)
ggplot_pie_chart_2

ggsave(ggplot_pie_chart_2, filename = 'Online_resources_nolines.png')


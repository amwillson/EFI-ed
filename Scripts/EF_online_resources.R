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
#use in combination with Data_cleaning_online_resources.R
rm(list = ls())

EF_online_resources_pt <- PivotTable$new() # create pivottable 
EF_online_resources_pt$addData(data) #populate with ef.data
EF_online_resources_pt$addColumnDataGroups("Category", addTotal = FALSE) #using Sub-Topic data to populate pivottable
EF_online_resources_pt$defineCalculation(calculationName="TotalCourses", summariseExpression="n()") #calculating number of courses
EF_online_resources_pt$renderPivot() #create pivot table 
EF_online_resources_df <- EF_online_resources_pt$asDataFrame()
colnames_EF_online <- data.frame(colnames(EF_online_resources_df))
EF_online_resources_df <- transpose(EF_online_resources_df)
EF_online_resources_df <- cbind(EF_online_resources_df, colnames_EF_online)
EF_online_resources_df <-EF_online_resources_df %>% dplyr::rename('Subtopics'= colnames.EF_online_resources_df., 'Number of Resources' = V1)

###Barplot###

barplot_online_resources <- ggplot(EF_online_resources_df, aes(x=reorder(Subtopics,`Number of Resources`), y=`Number of Resources`)) + geom_bar(position="stack", stat = "identity") + coord_flip() + labs(title = "Total Online Resources by Subtopic") + xlab("Subtopics")
barplot_online_resources

###Pie Chart###
color_count <- length(EF_online_resources_df$Subtopics)
coul <- brewer.pal(12, "Paired")
coul <- colorRampPalette(coul)(color_count)
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))

set.seed(2643598)                                             # Set random seed
palette3 <- sample(palette3_all, color_count)                    # Sample color

pie_chart_online_resources <- ggplot(EF_online_resources_df, aes(x="", y=`Number of Resources`, fill=Subtopics)) + geom_bar(stat="identity", width=1, color = "black") + coord_polar("y", start=0) + theme_void() + labs(title = "Total Resources by Suptopic")
pie_chart_online_resources + scale_fill_manual(values = coul)
pie_chart_online_resources + scale_fill_manual(values = palette3)


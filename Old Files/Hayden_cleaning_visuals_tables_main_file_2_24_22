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

row_names_to_remove<-c("Total") #had to remove total row because it was taking mean including total row
summary_subtopic_2 <- summary_subtopic[!(row.names(summary_subtopic) %in% row_names_to_remove),] #push to new dataset
mean_by_subtopic <- summary_subtopic_2 %>% summarise_if(is.numeric, mean) #calculate mean across all columns without total row
sd_by_subtopic <- summary_subtopic_2 %>% summarise_if(is.numeric, sd) #calculate sd across all rows
rownames(mean_by_subtopic) <- ("Mean") #renaming row name
rownames(sd_by_subtopic) <- ("sd") #renaming row name
sd_mean_subtopics <- rbind(summary_subtopic, mean_by_subtopic, sd_by_subtopic) #merge the the datasets/vectors
is.num <- sapply(sd_mean_subtopics, is.numeric) #
sd_mean_subtopics[is.num] <- lapply(sd_mean_subtopics[is.num], round, 2) #rounding all values in dataset to 2 decimals
#now going to add relevant variables back in like state, school classification, highest degree offered etc.
reduced_data <- dplyr::select(ef.data, -c(`Sub.topic`)) #removing columns from dataset
reduced_data <- distinct(reduced_data, College, .keep_all = TRUE) #this removes all duplicate rows based on college name, so now there should the same number of rows as distinct colleges
reduced_data <- reduced_data %>% arrange(College)
numeric_type <- data.frame(reduced_data$Type) 
must_convert<-sapply(numeric_type,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(numeric_type[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric

out<-cbind(numeric_type[,!must_convert],M2)
nrow_sd_mean_sub <- nrow(sd_mean_subtopics)
rows_remove <- seq(nrow_sd_mean_sub-2, nrow_sd_mean_sub)
sd_mean_subtopics <- sd_mean_subtopics %>% filter(!row_number() %in% rows_remove)
dff <- merge(data.frame(reduced_data, row.names=NULL), data.frame(sd_mean_subtopics, row.names=NULL), 
             by = 0, all = TRUE)[-1] #merging dataframes to create full dataframe with college data and summarized subtopic data
data_for_regression <- merge(data.frame(reduced_data, row.names=NULL), data.frame(summary_subtopic_2, row.names=NULL), 
                             by = 0, all = TRUE)[-1] #merge another dataset that is easier to use for the regression/BCLM

data_for_regression <- data_for_regression %>% arrange(College)

dff <- dff %>% arrange(College) #arranging rows in alphabetical order by college name 

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
length(EF_courses_subtopics)
colnames(Total)
colnames(EF_courses_subtopics)


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

### Bar plot ###

colnames <- data.frame(colnames(Total))
Total <- transpose(Total)
Total <- cbind(Total, colnames)
Total <- head(Total, -1)
Total <-Total %>% dplyr::rename('Subtopics'= colnames.Total., 'Number of Courses' = V1)
barplot_subtopic_total <- ggplot(Total, aes(x=reorder(Subtopics,`Number of Courses`), y=`Number of Courses`)) + geom_bar(position="stack", stat = "identity") + coord_flip() + labs(title = "Total Course by Subtopic") + xlab("Subtopics")
barplot_subtopic_total + scale_y_continuous(labels = scales::percent)



### Pie Charts ###

color_count <- length(Total$Subtopics)
coul <- brewer.pal(12, "Paired")
coul <- colorRampPalette(coul)(color_count)
getPalette = colorRampPalette(brewer.pal(12, "Set3"))

palette3_info <- brewer.pal.info[brewer.pal.info$category == "qual", ]  # Extract color info
palette3_all <- unlist(mapply(brewer.pal,                     # Create vector with all colors
                              palette3_info$maxcolors,
                              rownames(palette3_info)))

set.seed(2643598)                                             # Set random seed
palette3 <- sample(palette3_all, color_count)                    # Sample color

pie(Total$`Number of Courses`, labels = Total$`Number of Courses`, main = "Pie Chart of Course Distribution", col = rainbow(length(Total$`Number of Courses`))) + legend("topright", Total$Subtopics, cex = 0.8, fill = rainbow(length(Total$`Number of Courses`))) #basic pie chart

ggplot_pie_chart <- ggplot(Total, aes(x="", y=`Number of Courses`, fill=Subtopics)) + geom_bar(stat="identity", width=1, color = "black") + coord_polar("y", start=0) + theme_void() + labs(title = "Total Courses by Suptopic")
ggplot_pie_chart + scale_fill_grey(start = 0.1, end = .9)
ggplot_pie_chart + scale_fill_viridis(discrete = TRUE)
ggplot_pie_chart + scale_fill_manual(values = coul)
ggplot_pie_chart + scale_fill_manual(values = palette3)
### pie chart make color changes to make it easier to interpret 


###Map Visualization###
College_Locations <- read_excel('College Location Data.xlsx', sheet  = 'College Location Data Final')

course_total <- data.frame(summary_subtopic_2$Total)
transformed_data <- usmap_transform(College_Locations)
map_locations <- plot_usmap("states") + geom_point(data = transformed_data, aes(x = Longitude.1, y = Latitude.1), color = "red", size = 1)
map_locations
transformed_data <- transformed_data %>% arrange(College)
weighted.data <- cbind(transformed_data, course_total)

sort.df <- with(data_for_regression,  data_for_regression[order(College) , ])
Type <- data.frame(sort.df$Type)
weighted.data <- weighted.data[order(weighted.data$College) , ]
type_weighted_data <- cbind(weighted.data, Type)

type_weighted_data <-type_weighted_data %>% dplyr::rename('Total Courses'= summary_subtopic_2.Total, Type = sort.df.Type)

map_weighted_data <- plot_usmap("states") + geom_point(data = type_weighted_data, aes(x = Longitude.1, y = Latitude.1, size=`Total Courses`, color = Type))
map_weighted_data
us_scatter_type_courses <- map_weighted_data + theme(legend.position = "left") + labs(title = "Courses by College Type")
us_scatter_type_courses #finalized weighted map

### Map Visualization By Region ###

college_state <- data.frame(dff$State)
weighted_data_state <- cbind(weighted.data, college_state)

weighted_data_state$dff.State <- as.character(weighted_data_state$dff.State)

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


Region_summary <- weighted_data_state %>% tbl_summary(by = Status, statistic = all_continuous() ~ "{mean}")
summary(Region_summary)
midwest_summary <- data.frame(Region_summary$table_body$stat_1)
northeast_summary <- data.frame(Region_summary$table_body$stat_2)
south_summary <- data.frame(Region_summary$table_body$stat_3)
west_summary <- data.frame(Region_summary$table_body$stat_4)
region_table <- data.frame(c('Midwest', 'Northeast', 'South', 'West'))
averages_by_region <- cbind(midwest_summary, northeast_summary, south_summary, west_summary)
slice <- 55 + nrow(summary_subtopic_2)
averages_by_region <- averages_by_region %>% slice(slice)    
regions <- c('Midwest', 'Northeast', 'South', 'West')
regions <- data.frame(regions)
averages_by_region <- transpose(averages_by_region)
averages_by_region <- cbind(regions, averages_by_region) 


# source of code: https://gis.stackexchange.com/questions/403856/how-to-create-a-us-map-in-r-with-separation-between-states-when-using-albersusa
state_ISO2 <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
regions_census_main <- c("South", "West", "West", "South", "West", "West", "Northeast", "South", "South", "South", "West", "West", "Midwest", "Midwest", "Midwest", "Midwest", "South", "South", "Northeast", "South", "Northeast", "Midwest", "Midwest", "South", "Midwest", "West", "Midwest", "West", "Northeast", "Northeast", "West", "Northeast", "South", "Midwest", "Midwest", "South", "West", "Northeast", "Northeast", "South", "Midwest", "South", "South", "West", "Northeast", "South", "West", "South", "Midwest", "West")
regions_census_main_value <- rep(0, length(regions_census_main))
regions_census_main_value[regions_census_main == "West"] <- averages_by_region[4,2]
regions_census_main_value[regions_census_main == "Midwest"] <- averages_by_region[1,2]
regions_census_main_value[regions_census_main == "Northeast"] <- averages_by_region[2,2]
regions_census_main_value[regions_census_main == "South"] <- averages_by_region[3,2]
regions_census_main_value <- as.numeric(regions_census_main_value)

dataus <- as.data.frame(cbind(state_ISO2, regions_census_main, regions_census_main_value), stringsAsFactors = FALSE)

West <- averages_by_region[4,2]
Northeast <- averages_by_region[2,2]
Midwest <- averages_by_region[1,2]
South <- averages_by_region[3,2]


g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = plotly::toRGB('white')
)

region_plot <- plot_geo(dataus, locationmode = 'USA-states') %>%
  add_trace(
    z = regions_census_main_value, locations = ~state_ISO2,
    color = regions_census_main_value, colors = "Blues"
  ) %>%
  colorbar(title = "Average Number of Courses", data(regions_census_main_value), limits = c(15,60)) %>%
  layout(hovermode = FALSE,
    title = 'Average Number of Courses by Region',
    geo = g
  )
region_plot %>% add_annotations(bgcolor = "white", bordercolor = "black", borderwidth = 3, x = c(.25,.261,.55,.55,.85,.825,.62,.62), y = c(.52,.490,.55,.520,.60,.57,.42,.39), text = c("West", West, 
            "Midwest", Midwest, "Northeast",Northeast, "South", South), showarrow = c(FALSE, FALSE, FALSE, FALSE,FALSE, FALSE, FALSE, FALSE))


### Tiered Bar Plot###
pt.2 <- PivotTable$new() # create pivottable 
pt.2$addData(ef.data) #populate with ef.data
pt.2$addRowDataGroups("Sub.topic")
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
tiered_barplot <- ggplot((data.table), aes(fill=Type, y=Total, x=reorder(Subtopic, Total))) + geom_bar(position="stack", stat="identity") + coord_flip() + labs(title = "Course Distribution by Subtopic and Institution Type") + ylab("Total Number of Courses") + xlab("Subtopic")
tiered_percent_barplot <- tiered_barplot + scale_y_continuous(labels = scales::percent)
tiered_barplot
tiered_percent_barplot
Type_count <- data.frame(table(data_for_regression$Type))
data.table.2.year.technical <- data.table[data.table$Type == '2-Year Technical',]
data.table.2.year.technical$Total <- data.table.2.year.technical$Total/Type_count[1,2]

data.table.4.year.private <- data.table[data.table$Type == '4-Year Private',]
data.table.4.year.private$Total <- data.table.4.year.private$Total/Type_count[2,2]

data.table.4.year.public <- data.table[data.table$Type == '4-Year Public',]
data.table.4.year.public$Total <- data.table.4.year.public$Total/Type_count[3,2]

data.table.community.college <- data.table[data.table$Type == 'Community College',]
data.table.community.college$Total <- data.table.community.college$Total/Type_count[4,2]

data.table.for.profit <- data.table[data.table$Type == 'For Profit',]
data.table.for.profit$Total <- data.table.for.profit$Total/Type_count[5,2]

data.table.type.standardized <- rbind(data.table.4.year.private, data.table.2.year.technical,data.table.4.year.public, data.table.community.college, data.table.for.profit)

tiered_barplot_standardized <- ggplot((data.table.type.standardized), aes(fill=Type, y=Total, x=reorder(Subtopic, Total))) + geom_bar(position="stack", stat="identity") + coord_flip() + labs(title = "Course Distribution by Subtopic and Institution Type") + ylab("Total Number of Courses") + xlab("Subtopic")
tiered_barplot_standardized 

### Averages ###

  ### Table: Average Number of Courses by Suptopic by Institution Type
average_courses_by_int_type <- PivotTable$new()
average_courses_by_int_type$addData(ef.data)
average_courses_by_int_type$addColumnDataGroups("Type", addTotal = FALSE)
average_courses_by_int_type$addRowDataGroups("Sub.topic", addTotal = FALSE)
average_courses_by_int_type$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_courses_by_int_type$renderPivot()
average_courses_by_int_type <- average_courses_by_int_type$asDataFrame() #pushing pivottable to dataframe for easier use and analysis la
average_courses_by_int_type <- average_courses_by_int_type %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
average_courses_by_int_type$`2-Year Technical` <- average_courses_by_int_type$`2-Year Technical`/Type_count[1,2]
average_courses_by_int_type$`4-Year Private` <- average_courses_by_int_type$`4-Year Private`/Type_count[2,2]
average_courses_by_int_type$`4-Year Public` <- average_courses_by_int_type$`4-Year Public`/Type_count[3,2]
average_courses_by_int_type$`Community College` <- average_courses_by_int_type$`Community College`/Type_count[4,2]
average_courses_by_int_type$`For Profit` <- average_courses_by_int_type$`For Profit`/Type_count[5,2]
average_courses_by_int_type <- average_courses_by_int_type %>% mutate(across(where(is.numeric), ~ round(., 2)))


  ### Table: Average Number of Courses by Institution Type

average_course_type <- PivotTable$new()
average_course_type$addData(ef.data)
average_course_type$addColumnDataGroups("Type", addTotal = FALSE)
average_course_type$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_type$renderPivot()
average_course_type <- average_course_type$asDataFrame()
average_course_type <- average_course_type %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
average_course_type$`2-Year Technical` <- average_course_type$`2-Year Technical`/Type_count[1,2]
average_course_type$`4-Year Private` <- average_course_type$`4-Year Private`/Type_count[2,2]
average_course_type$`4-Year Public` <- average_course_type$`4-Year Public`/Type_count[3,2]
average_course_type$`Community College` <- average_course_type$`Community College`/Type_count[4,2]
average_course_type$`For Profit` <- average_course_type$`For Profit`/Type_count[5,2]
average_course_type <- average_course_type %>% mutate(across(where(is.numeric), ~ round(., 2)))


  ### Table: Average Number of Courses by Highest Degree Offered

average_course_degree <- PivotTable$new()
average_course_degree$addData(ef.data)
average_course_degree$addColumnDataGroups("Highest.degree.offered", addTotal = FALSE)
average_course_degree$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_degree$renderPivot()
average_course_degree <- average_course_degree$asDataFrame()
average_course_degree <- average_course_degree %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
Degree_count <- data.frame(table(data_for_regression$Highest.degree.offered))
average_course_degree$`AA/AS`<-average_course_degree$`AA/AS`/Degree_count[1,2]
average_course_degree$`BA/BS` <- average_course_degree$`BA/BS`/Degree_count[2,2]
average_course_degree$`MA/MS` <- average_course_degree$`MA/MS`/Degree_count[3,2]
average_course_degree$PhD <- average_course_degree$`PhD`/Degree_count[4,2]
average_course_degree$`Other Terminal Degree` <- average_course_degree$`Other Terminal Degree`/Degree_count[5,2]
average_course_degree <- average_course_degree %>% mutate(across(where(is.numeric), ~ round(., 2)))

  ### Table: Average number of courses by suptopic by highest degree offered

average_course_subtopic_degree <- PivotTable$new()
average_course_subtopic_degree$addData(ef.data)
average_course_subtopic_degree$addColumnDataGroups("Highest.degree.offered", addTotal = FALSE)
average_course_subtopic_degree$addRowDataGroups("Sub.topic", addTotal = FALSE)
average_course_subtopic_degree$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_subtopic_degree$renderPivot()
average_course_subtopic_degree <- average_course_subtopic_degree$asDataFrame() #pushing pivottable to dataframe for easier use and analysis la
average_course_subtopic_degree <- average_course_subtopic_degree %>% mutate(across(everything(), .fns = ~replace_na(.,0)))
average_course_subtopic_degree$`AA/AS`<-average_course_subtopic_degree$`AA/AS`/Degree_count[1,2]
average_course_subtopic_degree$`BA/BS` <- average_course_subtopic_degree$`BA/BS`/Degree_count[2,2]
average_course_subtopic_degree$`MA/MS` <- average_course_subtopic_degree$`MA/MS`/Degree_count[3,2]
average_course_subtopic_degree$PhD <- average_course_subtopic_degree$`PhD`/Degree_count[4,2]
average_course_subtopic_degree$`Other Terminal Degree` <- average_course_subtopic_degree$`Other Terminal Degree`/Degree_count[5,2]
average_course_subtopic_degree <- average_course_subtopic_degree %>% mutate(across(where(is.numeric), ~ round(., 2)))


  ### Table: Average number of courses by Carnegie Classification

average_course_carnegie <- PivotTable$new()
average_course_carnegie$addData(ef.data)
average_course_carnegie$addColumnDataGroups("Carnegie.classification.2", addTotal = FALSE)
average_course_carnegie$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_carnegie$renderPivot()
average_course_carnegie <- average_course_carnegie$asDataFrame()
carnegie_count <- data.frame(table(data_for_regression$Carnegie.classification.2))
average_course_carnegie$A <- average_course_carnegie$A/carnegie_count[1,2]
average_course_carnegie$`A/B` <- average_course_carnegie$`A/B`/carnegie_count[2,2]
average_course_carnegie$B <- average_course_carnegie$B/carnegie_count[3,2]
average_course_carnegie$`D/PU` <- average_course_carnegie$`D/PU`/carnegie_count[4,2]
average_course_carnegie$M1 <- average_course_carnegie$M1/carnegie_count[5,2]
average_course_carnegie$M3 <- average_course_carnegie$M3/carnegie_count[6,2]
average_course_carnegie$R1 <- average_course_carnegie$R1/carnegie_count[7,2]
average_course_carnegie$R2 <- average_course_carnegie$R2/carnegie_count[8,2]
average_course_carnegie$TC <- average_course_carnegie$TC/carnegie_count[9,2]
average_course_carnegie <- average_course_carnegie %>% mutate(across(where(is.numeric), ~ round(., 2)))


  ### Table: Average Number of courses by subtopic by carnegie classification
average_course_subtopic_carnegie <- PivotTable$new()
average_course_subtopic_carnegie$addData(ef.data)
average_course_subtopic_carnegie$addColumnDataGroups("Carnegie.classification.2", addTotal = FALSE)
average_course_subtopic_carnegie$addRowDataGroups("Sub.topic", addTotal = FALSE)
average_course_subtopic_carnegie$defineCalculation(calculationName = "TotalCourses", summariseExpression = "n()")
average_course_subtopic_carnegie$renderPivot()
average_course_subtopic_carnegie <- average_course_subtopic_carnegie$asDataFrame()
average_course_subtopic_carnegie[is.na(average_course_subtopic_carnegie)] <- 0
average_course_subtopic_carnegie$`A` <- average_course_subtopic_carnegie$`A`/carnegie_count[1,2]
average_course_subtopic_carnegie$`A/B` <- average_course_subtopic_carnegie$`A/B`/carnegie_count[2,2]
average_course_subtopic_carnegie$`B` <- average_course_subtopic_carnegie$`B`/carnegie_count[3,2]
average_course_subtopic_carnegie$`D/PU` <- average_course_subtopic_carnegie$`D/PU`/carnegie_count[4,2]
average_course_subtopic_carnegie$`M1` <- average_course_subtopic_carnegie$`M1`/carnegie_count[5,2]
average_course_subtopic_carnegie$`M3` <- average_course_subtopic_carnegie$`M3`/carnegie_count[6,2]
average_course_subtopic_carnegie$`R1` <- average_course_subtopic_carnegie$`R1`/carnegie_count[7,2]
average_course_subtopic_carnegie$`R2` <- average_course_subtopic_carnegie$`R2`/carnegie_count[8,2]
average_course_subtopic_carnegie$`TC` <- average_course_subtopic_carnegie$`TC`/carnegie_count[9,2]
average_course_subtopic_carnegie <- average_course_subtopic_carnegie %>% dplyr::mutate(across(where(is.numeric), ~ round(., 2)))



## This script downloads, reads,  and randomizes a list of higher education institutions in the USA
## The list of institutions is used to identify courses related to ecological forecasting
## in a random subsample of institutions in the USA

## Author: AM Willson
## Date modified: 31 January 2022

rm(list = ls())

library(xml2)
library(rvest)

url = "https://www.theedadvocate.org/an-a-z-list-of-u-s-colleges-and-universities/"

page = read_html(url)  
table = html_table(page, fill = T)

data = table[[1]]
rm(table, page)

colnames(data) = data[1,]
data = data[-1,]

## Add random number next to each row without replacement
data$index = sample(1:nrow(data), nrow(data), replace = F)
data = data[order(data$index),]

write.csv(x = data, file = 'Data/College_list.csv')

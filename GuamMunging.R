library(tidyr)
library(lubridate)

setwd("D:/Iowa State University/2016 Fall/R class/Data manipulation")
my.data <- read.csv(file = "FW_master_2016_foranalysis.csv", header = TRUE, stringsAsFactors=F)

str(my.data)
unique(my.data$island)
my.data$island <- gsub("guam ", "guam", my.data$island)
my.data$island <- gsub("Saipan", "saipan", my.data$island)
my.data$island <- gsub("Rota", "rota", my.data$island)
my.data$island <- as.factor(my.data$island)
levels(my.data$island)

unique(my.data$site)
my.data$site <- tolower(my.data$site)
my.data$site <- gsub("blas", "nblas", my.data$site)
my.data$site <- gsub("nnblas", "nblas", my.data$site)
my.data$site <- gsub("palii_w", "paliiw", my.data$site)
my.data$site <- gsub("snblas", "sblas", my.data$site)
my.data$site <- as.factor(my.data$site)
levels(my.data$site)

my.data$date <- dmy(my.data$date)

my.data$observer <- tolower(my.data$observer)




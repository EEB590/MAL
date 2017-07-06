library(lme4)
library(lsmeans)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

flowers <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/TotalFlowersPerPlant.csv", header = T, as.is = T)


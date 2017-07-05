library(dplyr)
library(tidyr)
library(lubridate)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

bp1 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160528.csv", header = T, as.is = T)
bp2 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160531.csv", header = T, as.is = T)
bp3 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160603.csv", header = T, as.is = T)
bp4 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160606.csv", header = T, as.is = T)
bp5 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160609.csv", header = T, as.is = T)
bp6 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160612.csv", header = T, as.is = T)
bp7 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160615.csv", header = T, as.is = T)
bp8 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160622.csv", header = T, as.is = T)
bp9 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160625.csv", header = T, as.is = T)

bpall <- as.data.frame(rbind(bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8, bp9))

bpall <- bpall[,-c(12:1002)]
rm(bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8, bp9)

#create unique plant ID
bpall$plantid <- paste(bpall$Plot.ID, bpall$Plant.ID, sep = "-")

#create treatment category
bpall$treatment <- gsub("^C", "", bpall$Plot.ID)
bpall$treatment <- gsub("W", "", bpall$treatment)
bpall$treatment <- gsub("E", "", bpall$treatment)
bpall$treatment <- gsub("HSR", "H", bpall$treatment)
bpall$treatment <- gsub("SR", "C", bpall$treatment)
bpall$treatment <- gsub('[[:digit:]]+', '', bpall$treatment)

#remove plants that have no flowers
bpsmall <- bpall[!(is.na(bpall$Flower.height)),]
bpsmall <- bpsmall[bpsmall$Flower.height != "0",]
bpsmall <- bpsmall[(!is.na(bpsmall$Leaf.height)),]  #three leaf heights were recorded as flower heights for plants with no flowers - remove
rm(bpall)
bpsmall <- bpsmall[,-c(2,4:5, 7, 11)]

bpsmall <- rename(bpsmall, plot = Plot.ID, date = Date, BD = Buds, FF = Fully.flowering, SG = Senescing, SD = Scenesced)

bpsmall <- bpsmall[,c(1,7,8,2,3,4,5,6)]

bpsmall$date <- mdy(bpsmall$date)
bpsmall$plot <- as.factor(bpsmall$plot)
bpsmall$plantid <- as.factor(bpsmall$plantid)
bpsmall$treatment <- as.factor(bpsmall$treatment)

bpsmall[is.na(bpsmall)] <- 0
bpsmall$TF <- rowSums(bpsmall[,5:8])

#Create plant-level tables

plantdfs <- list()
dfnames <- levels(bpsmall$plantid)
for (i in 1:length(dfnames)) {
  plantdfs[[i]] = as.data.frame(subset(bpsmall, plantid == dfnames[i], select = c(1:9)))
}

for (i in 1:length(plantdfs)){
  print(plantdfs[[i]])
}

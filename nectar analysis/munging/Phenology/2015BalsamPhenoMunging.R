library(tidyr)
library(dplyr)
library(lubridate)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

bp15all <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/2015BalsamrootPhenologyFinal.csv", header = T, as.is = T)

#Convert from wide to long format

bp15 <- bp15all[,1:12]

for (i in 1:15) {
  bptemp <- bp15all[,c(1:2,13:22)]
  names(bptemp)[3:12] <- c("Date", "Flower.height", "Leaf.height", "BD", "AB", "FF", "SG", "SD", "ST", "notes")
  bp15 <- rbind(bp15, bptemp)
  bp15all <- bp15all[,-c(13:22)]
}

rm(bp15all)
rm(bptemp)

#create unique plant ID
bp15$plantid <- paste(bp15$Plot, bp15$Plant, sep = "-")

#create treatment category
bp15$treatment <- gsub("^C", "", bp15$Plot)
bp15$treatment <- gsub("W", "", bp15$treatment)
bp15$treatment <- gsub("E", "", bp15$treatment)
bp15$treatment <- gsub("HSR", "H", bp15$treatment)
bp15$treatment <- gsub("SR", "C", bp15$treatment)
bp15$treatment <- substr(bp15$treatment, 1, 1)

#remove plants that have no flowers
bpsmall <- bp15[!(is.na(bp15$Flower.height)),]
bpsmall <- bpsmall[bpsmall$Flower.height != "0",]
rm(bp15)
bpsmall <- bpsmall[,-c(2,4:5, 7, 11:12)]

bpsmall <- rename(bpsmall, plot = Plot, date = Date)

bpsmall <- bpsmall[,c(1,7,8,2,3,4,5,6)]

bpsmall$date <- mdy(bpsmall$date)
bpsmall$plot <- as.factor(bpsmall$plot)
bpsmall$plantid <- as.factor(bpsmall$plantid)
bpsmall$treatment <- as.factor(bpsmall$treatment)

bpsmall[is.na(bpsmall)] <- 0
bpsmall$TF <- rowSums(bpsmall[,5:8])


library(lubridate)
library(tidyr)

bals15 <- read.csv("nectar analysis/raw data/2015 Balsamroot Nectar Data raw.csv", header = T, as.is = T)

#get rid of wind, temp, humidity, shade/sun, time, and notes
bals15 <- bals15[,-c(2,3,4,5,6,20)]

bals15$date <- mdy(bals15$date)

# Get the plots properly labelled
plots <- as.data.frame(strsplit(bals15$plot, split = " "))
plots <- t(plots)
plots <- as.data.frame(plots)
colnames(plots) <- c("name", "number")
plots$no <- gsub("\\(", "", plots$number)
plots$number <- gsub("\\)", "", plots$no)
plots <- plots[,-3]
plots$treat <- gsub("^C", "", plots$name)
plots$treat <- gsub("W", "", plots$treat)
plots$treat <- gsub("E", "", plots$treat)
plots$treat <- gsub("HSR", "H", plots$treat)
plots$treat <- gsub("SR", "C", plots$treat)
plots$treatment <- gsub("C", "0", plots$treat)
plots$treatment <- gsub("H", "1", plots$treatment)

bals15$plotno <- as.factor(plots$number)
bals15$plotname <- as.factor(plots$name)
bals15$treatment <- as.factor(plots$treatment)
bals15 <- bals15[,c(1,2,16,15,3,4,5,6,7,8,9,10,11,12,13,14)]

# calculate volume
vols <- as.data.frame(bals15[,c("cm.of.tube.filled", "cm.of.second.tube.filled", "cm.of.third.tube", "cm.of.fourth.tube", "length.tube..mm.")])
vols$totalvol <- ((rowSums(vols[1:4], na.rm = T))*10)/vols$length.tube..mm.

bals15$volume <- vols$totalvol
bals15 <- bals15[,-c(7:10)]

# calculate BRIX
bals15$X..sucrose <- as.numeric(bals15$X..sucrose) #this coerces the >50 and <45 into NAs
bals15$BRIX <- rowMeans(bals15[,9:12], na.rm = T)
bals15 <- bals15[,-c(9:12)]

#calculate sugar mass
ref <- read.csv("nectar analysis/analysis/concentration reference.csv", header = T)
ref<- ref[,c(1,8)]
colnames(ref) <- c("BRIX", "mass")
bals15$BRIX <- gsub("NaN", "0", bals15$BRIX)
bals15$BRIX <- as.numeric(bals15$BRIX)
balsam15 <- merge(bals15, ref, by = "BRIX")
balsam15 <- balsam15[,c(2,3,4,5,6,7,8,9,10,1,11)]


#balsam15 <- order(balsam15$date)

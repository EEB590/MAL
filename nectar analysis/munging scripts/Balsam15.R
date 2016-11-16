library(lubridate)
library(tidyr)

bals15 <- read.csv("nectar analysis/raw data/2015 Balsamroot Nectar Data raw.csv", header = T, as.is = T)

#get rid of wind, temp, humidity, shade/sun, time, and notes
bals15 <- subset(bals15, select = -c(avg.wind..mph., temp...F., Humidity...., Shade..Sun, time..am., notes))

bals15$date <- mdy(bals15$date)

# Get the plots properly labelled
plots <- as.data.frame(strsplit(bals15$plot, split = " "))
plots <- t(plots)
plots <- as.data.frame(plots)
colnames(plots) <- c("name", "number")
plots$no <- gsub("\\(", "", plots$number)
plots$number <- gsub("\\)", "", plots$no)
plots$plot <- paste(plots$name, plots$number, sep = "")
plots <- subset(plots, select = -c(no))
plots$treat <- gsub("^C", "", plots$name)
plots$treat <- gsub("W", "", plots$treat)
plots$treat <- gsub("E", "", plots$treat)
plots$treat <- gsub("HSR", "H", plots$treat)
plots$treat <- gsub("SR", "C", plots$treat)

bals15$plot <- as.factor(plots$plot)
bals15$treatment <- as.factor(plots$treat)
bals15$plant <- paste(bals15$plot,bals15$Plant..,sep = "-")
bals15 <- subset(bals15, select = -Plant..)


# calculate volume
vols <- as.data.frame(bals15[,c("cm.of.tube.filled", "cm.of.second.tube.filled", "cm.of.third.tube", "cm.of.fourth.tube", "length.tube..mm.")])
vols$totalvol <- ((rowSums(vols[1:4], na.rm = T))*10)/vols$length.tube..mm.

bals15$volume <- vols$totalvol
bals15 <- subset(bals15, select = -c(cm.of.tube.filled, cm.of.second.tube.filled, cm.of.third.tube, cm.of.fourth.tube, size.tube..µL., length.tube..mm.))

# calculate BRIX
bals15$X..sucrose <- as.numeric(bals15$X..sucrose) #this coerces the >50 and <45 into NAs
bals15$BRIX <- rowMeans(bals15[,c("X..sucrose","X..of.second.tube","X..of.third.tube","X..of.fourth.tube")], na.rm = T)
bals15 <- subset(bals15, select = -c(X..sucrose,X..of.second.tube,X..of.third.tube,X..of.fourth.tube))

#calculate sugar mass
ref <- read.csv("nectar analysis/analysis/concentration reference.csv", header = T)
ref<- ref[,c(1,8)]  #Look-up table to convert BRIX to sugar concentration (mg/mL)
colnames(ref) <- c("BRIX", "conc")
bals15$BRIX <- gsub("NaN", "0", bals15$BRIX)
bals15$BRIX <- as.numeric(bals15$BRIX)
balsam15 <- merge(bals15, ref, by = "BRIX")
balsam15$mass <- balsam15$volume*balsam15$conc*0.001 #calculate raw mass from volume and concentration
balsam15 <- balsam15[,c(2,3,4,5,6,1,8)]
balsam15 <- balsam15[order(balsam15$date),]

#subset for volume, get rid of 0's
balsvol15 <- subset(balsam15, volume != 0, select = c(date, plot, treatment, plant, volume))
write.csv(balsvol15, file = "nectar analysis/data files/balsvol15.csv")

#subset for sugar, get rid of 0's
balssugar15 <- subset(balsam15, BRIX != 0, select = c(date, plot, treatment, plant, BRIX, mass))
write.csv(balssugar15, file = "nectar analysis/data files/balssugar15.csv")

library(lubridate)
library(tidyr)

bals16 <- read.csv("nectar analysis/raw data/2016 Balsamroot Nectar Data raw data FINAL corrected.csv", header = T, as.is = T)

#get rid of empty rows, plus wind, temp, humidity, shade/sun, time, and notes
bals16 <- subset(bals16[!is.na(bals16$Plant..),])
bals16 <- subset(bals16, select = -c(avg.wind..mph., temp...F., Humidity...., Shade..Sun, time..am., Volume..µL., notes))

bals16$date <- mdy(bals16$date)

# Get the plots properly labelled
bals16$treatment <- as.factor(gsub("[[:digit:]]", "", bals16$plot))
bals16$treatment <- gsub("^C", "", bals16$treatment)
bals16$treatment <- gsub("W", "", bals16$treatment)
bals16$treatment <- gsub("E", "", bals16$treatment)
bals16$treatment <- gsub("HSR", "H", bals16$treatment)
bals16$treatment <- gsub("SR", "C", bals16$treatment)

bals16$plant <- as.factor(paste(bals16$plot,bals16$Plant..,sep = "-"))
bals16 <- subset(bals16, select = -Plant..)
bals16$plot <- as.factor(bals16$plot)
bals16$treatment <- as.factor(bals16$treatment)

# calculate volume
bals16$volume <- (bals16$mm.of.tube.filled/55)/bals16$size.tube..µL.
#all tubes used were 55mm in length (see scanned data sheets)
bals16 <- subset(bals16, select = -c(mm.of.tube.filled, size.tube..µL.))

#calculate sugar mass
ref <- read.csv("nectar analysis/analysis/concentration reference.csv", header = T)
ref<- ref[,c(1,8)]  #Look-up table to convert BRIX to sugar concentration (mg/mL)
colnames(ref) <- c("BRIX", "conc")

bals16$BRIX <- as.numeric(bals16$BRIX)
bals16$BRIX[is.na(bals16$BRIX)] <- 0
balsam16 <- merge(bals16, ref, by = "BRIX")

balsam16$mass <- balsam16$volume*balsam16$conc*0.001 #calculate raw mass from volume and concentration
balsam16 <- balsam16[,c(2,3,4,5,6,1,8)]
balsam16 <- balsam16[order(balsam16$date),]

#subset for volume, get rid of 0's
balsvol16 <- subset(balsam16, volume != 0, select = c(date, plot, treatment, plant, volume))
write.csv(balsvol16, file = "nectar analysis/data files/balsvol16.csv", row.names = FALSE)

#subset for sugar, get rid of 0's
balssugar16 <- subset(balsam16, BRIX != 0, select = c(date, plot, treatment, plant, BRIX, mass))
write.csv(balssugar16, file = "nectar analysis/data files/balssugar16.csv", row.names = FALSE)

library(lubridate)
library(tidyr)

bals15 <- read.csv("nectar analysis/raw data/2015 Balsamroot Nectar Data raw.csv", header = T, as.is = T)

#get rid of wind, temp, humidity, shade/sun, time, and notes
bals15 <- subset(bals15, select = -c(avg.wind..mph., temp...F., Humidity...., Shade..Sun, time..am., notes))

bals15$date <- mdy(bals15$date)

# Get the plots properly labelled and formatted
bals15 <- separate(bals15, plot, c("plotname", "plotno"), " ")
bals15$plotno <- gsub("\\(", "", bals15$plotno)
bals15$plotno <- gsub("\\)", "", bals15$plotno)

bals15$treatment <- gsub("^C", "", bals15$plotname)
bals15$treatment <- gsub("W", "", bals15$treatment)
bals15$treatment <- gsub("E", "", bals15$treatment)
bals15$treatment <- gsub("HSR", "H", bals15$treatment)
bals15$treatment <- gsub("SR", "C", bals15$treatment)

bals15 <- unite(bals15, plot, plotname, plotno, sep = "")

bals15$plot <- as.factor(bals15$plot)
bals15$treatment <- as.factor(bals15$treatment)
bals15$plant <- as.factor(paste(bals15$plot,bals15$Plant..,sep = "-"))
bals15 <- subset(bals15, select = -Plant..)


# calculate volume
vols <- as.data.frame(bals15[,c("cm.of.tube.filled", "cm.of.second.tube.filled", "cm.of.third.tube", "cm.of.fourth.tube", "size.tube..µL.", "length.tube..mm.")])

vols$vol1 <- ((vols$cm.of.tube.filled*10)/vols$length.tube..mm.)*vols$size.tube..µL.
vols$vol2 <- ((vols$cm.of.second.tube.filled*10)/vols$length.tube..mm.)*vols$size.tube..µL.
vols$vol3 <- ((vols$cm.of.third.tube*10)/vols$length.tube..mm.)*vols$size.tube..µL.
vols$vol4 <- ((vols$cm.of.fourth.tube*10)/vols$length.tube..mm.)*vols$size.tube..µL.

vols$totalvol <- ((rowSums(vols[7:10], na.rm = T)))

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
write.csv(balsvol15, file = "nectar analysis/data files/balsvol15.csv", row.names = FALSE)

#subset for sugar, get rid of 0's
balssugar15 <- subset(balsam15, BRIX != 0, select = c(date, plot, treatment, plant, BRIX, mass))
write.csv(balssugar15, file = "nectar analysis/data files/balssugar15.csv", row.names = FALSE)

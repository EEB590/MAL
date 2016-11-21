library(lubridate)
library(tidyr)

buck15 <- read.csv("nectar analysis/raw data/2015 Buckwheat Nectar Data v1 original.csv", header = T, as.is = T)

#get rid of wind, temp, humidity, shade/sun, time, volume, and notes
buck15 <- subset(buck15, select = -c(avg.wind..km.h., temp...C., Humidity...., Shade..Sun, time..am., Volume..µL., notes))

buck15$date <- mdy(buck15$date)

# Get the plots properly labelled and formatted
buck15 <- separate(buck15, plot, c("plotname", "plotno"), " ")
buck15$plotno <- gsub("\\(", "", buck15$plotno)
buck15$plotno <- gsub("\\)", "", buck15$plotno)

buck15$treatment <- gsub("^C", "", buck15$plotname)
buck15$treatment <- gsub("W", "", buck15$treatment)
buck15$treatment <- gsub("E", "", buck15$treatment)
buck15$treatment <- gsub("HSR", "H", buck15$treatment)
buck15$treatment <- gsub("SR", "C", buck15$treatment)

buck15 <- unite(buck15, plot, plotname, plotno, sep = "")

buck15$quad <- as.factor(paste(buck15$plot, buck15$Quadrant.of.plot, sep = ""))
buck15 <- subset(buck15, select = -Quadrant.of.plot)

buck15$plot <- as.factor(buck15$plot)
buck15$treatment <- as.factor(buck15$treatment)

# calculate volume
vols <- as.data.frame(buck15[,c("cm.of.tube.filled", "tube.2.cm", "tube.3.cm", "tube.4.cm", "tube.5.cm", "size.tube..µL.","size.tube..mm.")])
vols$vol1 <- ((vols$cm.of.tube.filled*10)/vols$size.tube..mm.)*vols$size.tube..µL.
vols$vol2 <- ((vols$tube.2.cm*10)/vols$size.tube..mm.)*vols$size.tube..µL.
vols$vol3 <- ((vols$tube.3.cm*10)/vols$size.tube..mm.)*vols$size.tube..µL.
vols$vol4 <- ((vols$tube.4.cm*10)/vols$size.tube..mm.)*vols$size.tube..µL.
vols$vol5 <- ((vols$tube.5.cm*10)/vols$size.tube..mm.)*vols$size.tube..µL.

vols$totalvol <- ((rowSums(vols[8:12], na.rm = T)))

buck15$volume <- vols$totalvol

buck15 <- subset(buck15, select = -c(cm.of.tube.filled, tube.2.cm, tube.3.cm, tube.4.cm, tube.5.cm, size.tube..µL.,size.tube..mm.))

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

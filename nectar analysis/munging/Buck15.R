library(lubridate)
library(tidyr)

buck15 <- read.csv("nectar analysis/data files/raw data/2015 Buckwheat Nectar Data v1 original.csv", header = T, as.is = T)

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
buck15$X..sucrose..tube.1. <- as.numeric(buck15$X..sucrose..tube.1.) #this coerces the >50 and <45 into NAs
buck15$BRIX <- rowMeans(buck15[,c("X..sucrose..tube.1.","tube.2..","tube.3..","tube.4..", "tube.5..")], na.rm = T)
buck15 <- subset(buck15, select = -c(X..sucrose..tube.1.,tube.2..,tube.3..,tube.4.., tube.5..))

#calculate sugar mass
ref <- read.csv("nectar analysis/analysis/concentration reference.csv", header = T)
ref<- ref[,c(1,8)]  #Look-up table to convert BRIX to sugar concentration (mg/mL)
colnames(ref) <- c("BRIX", "conc")
buck15$BRIX <- gsub("NaN", "0", buck15$BRIX)
buck15$BRIX <- as.numeric(buck15$BRIX)
buck15$BRIX <- round(buck15$BRIX)
buckwt15 <- merge(buck15, ref, by = "BRIX")
buckwt15$mass <- buckwt15$volume*buckwt15$conc*0.001 #calculate raw mass from volume and concentration
buckwt15 <- buckwt15[,c(2,3,4,5,6,1,8)]
buckwt15 <- buckwt15[order(buckwt15$date),]

write.csv(buckwt15, file = "nectar analysis/data files/buckwt15.csv", row.names = FALSE)

#subset for volume, get rid of 0's
buckvol15 <- subset(buckwt15, volume != 0, select = c(date, plot, treatment, quad, volume))
write.csv(buckvol15, file = "nectar analysis/data files/buckvol15.csv", row.names = FALSE)

#subset for sugar, get rid of 0's
bucksugar15 <- subset(buckwt15, BRIX != 0, select = c(date, plot, treatment, quad, BRIX, mass))
write.csv(bucksugar15, file = "nectar analysis/data files/bucksugar15.csv", row.names = FALSE)

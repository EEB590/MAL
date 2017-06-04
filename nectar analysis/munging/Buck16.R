library(lubridate)
library(tidyr)

buck16 <- read.csv("nectar analysis/data files/raw data/2016 Buckwheat Nectar Data raw data FINAL.csv", header = T, as.is = T)

#get rid of empty rows, plus wind, temp, humidity, shade/sun, time, and notes
buck16 <- subset(buck16[!is.na(buck16$plot),])
buck16 <- subset(buck16, select = -c(avg.wind..km.h., temp...C., Humidity, Shade..Sun, time..am., Volume..µL., notes, X, X.1))

buck16$date <- mdy(buck16$date)

# Get the plots properly labelled

#Note - plot labels on data sheets were numbers only (not location or treatment).  Using Diane's numbering system (not Matt's numbering system) to identify plots and treatment.

buck16$treatment <- buck16$plot
buck16$treatment <- gsub("12","H", buck16$treatment)
buck16$treatment <- gsub("11","C", buck16$treatment)
buck16$treatment <- gsub("10","C", buck16$treatment)
buck16$treatment <- gsub("9","H", buck16$treatment)
buck16$treatment <- gsub("8","H", buck16$treatment)
buck16$treatment <- gsub("7","C", buck16$treatment)
buck16$treatment <- gsub("6","C", buck16$treatment)
buck16$treatment <- gsub("5","H", buck16$treatment)
buck16$treatment <- gsub("4","H", buck16$treatment)
buck16$treatment <- gsub("3","C", buck16$treatment)
buck16$treatment <- gsub("2","C", buck16$treatment)
buck16$treatment <- gsub("1","H", buck16$treatment)
buck16$treatment <- as.factor(buck16$treatment)

buck16$plotna <- buck16$plot
buck16$plotna <- gsub("12", "WH", buck16$plotna)
buck16$plotna <- gsub("11", "WC", buck16$plotna)
buck16$plotna <- gsub("10", "WSR", buck16$plotna)
buck16$plotna <- gsub("9", "WHSR", buck16$plotna)
buck16$plotna <- gsub("8", "CHSR", buck16$plotna)
buck16$plotna <- gsub("7", "CSR", buck16$plotna)
buck16$plotna <- gsub("6", "CC", buck16$plotna)
buck16$plotna <- gsub("5", "CH", buck16$plotna)
buck16$plotna <- gsub("4", "EH", buck16$plotna)
buck16$plotna <- gsub("3", "EC", buck16$plotna)
buck16$plotna <- gsub("2", "ESR", buck16$plotna)
buck16$plotna <- gsub("1", "EHSR", buck16$plotna)

buck16$plot <- paste(buck16$plotna, buck16$plot, sep = "")
buck16$plot <- as.factor(buck16$plot)
buck16 <- subset(buck16, select = -plotna)

buck16$quad <- paste(buck16$plot, buck16$Quadrant.of.plot, sep = "")
buck16 <- subset(buck16, select = -Quadrant.of.plot)

# calculate volume

#discrepincy in data sheets about the length of the tube used.  Beginning sheets read 55mm, but corrected to 33mm on 160623Buckwheatp5.jpeg.  Using 33mm as the length for all readings.  All tubes total volume = 0.5 microliters

buck16$mm.of.tube.filled <- as.numeric(buck16$mm.of.tube.filled)
buck16$mm.of.tube.filled[is.na(buck16$mm.of.tube.filled)] <- 0
buck16$volume <- (buck16$mm.of.tube.filled/33)*0.5
buck16 <- subset(buck16, select = -c(mm.of.tube.filled, size.tube..µL.))

#calculate sugar mass
ref <- read.csv("nectar analysis/analysis/concentration reference.csv", header = T)
ref<- ref[,c(1,8)]  #Look-up table to convert BRIX to sugar concentration (mg/mL)
colnames(ref) <- c("BRIX", "conc")

buck16$BRIX <- as.numeric(buck16$BRIX)
buck16$BRIX[is.na(buck16$BRIX)] <- 0
buckwt16 <- merge(buck16, ref, by = "BRIX")

buckwt16$mass <- buckwt16$volume*buckwt16$conc*0.001 #calculate raw mass from volume and concentration
buckwt16 <- buckwt16[,c(2,3,4,5,6,1,8)]
buckwt16 <- buckwt16[order(buckwt16$date),]

write.csv(buckwt16, file = "nectar analysis/data files/buckwt16.csv", row.names = FALSE)

#subset for volume, get rid of 0's
buckvol16 <- subset(buckwt16, volume != 0, select = c(date, plot, treatment, quad, volume))
write.csv(buckvol16, file = "nectar analysis/data files/buckvol16.csv", row.names = FALSE)

#subset for sugar, get rid of 0's
bucksugar16 <- subset(buckwt16, BRIX != 0, select = c(date, plot, treatment, quad, BRIX, mass))
write.csv(bucksugar16, file = "nectar analysis/data files/bucksugar16.csv", row.names = FALSE)

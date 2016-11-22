library(lubridate)
library(tidyr)

buck16 <- read.csv("nectar analysis/raw data/2016 Buckwheat Nectar Data raw data FINAL.csv", header = T, as.is = T)

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

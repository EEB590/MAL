library(lubridate)
library(tidyr)

#Munge 2015 for compatibility with 2016 data
bals15 <- read.csv("nectar analysis/data files/raw data/2015 Balsamroot Nectar Data raw.csv", header = T, as.is = T)

  # calculate volume
vols <- as.data.frame(bals15[,c("cm.of.tube.filled", "cm.of.second.tube.filled", "cm.of.third.tube", "cm.of.fourth.tube", "size.tube..µL.", "length.tube..mm.")])

vols$vol1 <- ((vols$cm.of.tube.filled*10)/vols$length.tube..mm.)*vols$size.tube..µL.
vols$vol2 <- ((vols$cm.of.second.tube.filled*10)/vols$length.tube..mm.)*vols$size.tube..µL.
vols$vol3 <- ((vols$cm.of.third.tube*10)/vols$length.tube..mm.)*vols$size.tube..µL.
vols$vol4 <- ((vols$cm.of.fourth.tube*10)/vols$length.tube..mm.)*vols$size.tube..µL.

vols$totalvol <- ((rowSums(vols[7:10], na.rm = T)))

bals15$volume <- vols$totalvol
bals15 <- subset(bals15, select = -c(cm.of.tube.filled, cm.of.second.tube.filled, cm.of.third.tube, cm.of.fourth.tube, size.tube..µL., length.tube..mm.))
rm(vols)

  # calculate BRIX data so we have NAs for 0 volume and censored BRIX values
bals15$X..sucrose[which(bals15$volume == 0)] <- NA #set BRIX reading for 0 volume to NA
bals15$X..sucrose <- as.numeric(bals15$X..sucrose) #this coerces the >50 and <45 into NAs

bals15$BRIX <- rowMeans(bals15[,c("X..sucrose","X..of.second.tube","X..of.third.tube","X..of.fourth.tube")], na.rm = T)
bals15 <- subset(bals15, select = -c(X..sucrose,X..of.second.tube,X..of.third.tube,X..of.fourth.tube))

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

bals15 <- bals15[,c(1,6,2:5,7:8, 12, 10, 11, 9)]

#munge 2016 data
bals16 <- read.csv("nectar analysis/data files/raw data/2016 Balsamroot Nectar Data raw data FINAL corrected.csv", header = T, as.is = T)

  # calculate volume for 2016
bals16$volume <- (bals16$mm.of.tube.filled/55)*bals16$size.tube..µL.  #all tubes used were 55mm in length (see scanned data sheets)

  #mung the BRIX data so we have NAs for 0 volume and censored BRIX values
bals16$BRIX[which(bals16$volume == 0)] <- NA #set BRIX reading for 0 volume to NA
bals16$BRIX <- as.numeric(bals16$BRIX) #this coerces the "no reading" into NAs

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

bals16 <- bals16[,c(1, 6, 2:5, 7:8, 15, 14, 11, 13)]

balsam.df <- rbind(bals15, bals16); rm(bals15, bals16)

#munge the complete data frame
  #deal with data entry error in line 391 and at the end of the file
balsam.df[391,10] <- 0
balsam.df <- balsam.df[complete.cases(balsam.df$volume),]

balsam.df <- balsam.df[,-5] # no data for humidity in 2015
names(balsam.df)[c(2,3,4,5)] <- c("time", "wind", "temp", "shade_sun")
balsam.df$date <- mdy(balsam.df$date)

balsam.df$shade_sun <- gsub("S", "s", balsam.df$shade_sun)
balsam.df$shade_sun <- as.factor(balsam.df$shade_sun)

#NOTE FOR ANALYSIS
  #keep volumes that equal zero (i.e., empty flowers)
  #BRIX NA for either no nectar or censored data (past detection limit)

write.csv(balsam.df, file = "nectar analysis/munging/balsamroot_df.Rdata", row.names = FALSE)
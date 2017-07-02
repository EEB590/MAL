library(dplyr)
library(tidyr)
library(lubridate)

bp1 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160528.csv", header = T, as.is = T)
bp2 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160531.csv", header = T, as.is = T)
bp3 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160603.csv", header = T, as.is = T)
bp4 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160606.csv", header = T, as.is = T)
bp5 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160609.csv", header = T, as.is = T)
bp6 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160612.csv", header = T, as.is = T)
bp7 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160615.csv", header = T, as.is = T)
bp8 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160622.csv", header = T, as.is = T)
bp9 <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/160625.csv", header = T, as.is = T)

bpall <- as.data.frame(rbind(bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8, bp9))

bpall <- bpall[,-c(12:1002)]
rm(bp1, bp2, bp3, bp4, bp5, bp6, bp7, bp8, bp9)

#create unique plant ID
bpall$plantid <- paste(bpall$Plot.ID, bpall$Plant.ID, sep = "-")

#create treatment category
bpall$treatment <- gsub("^C", "", bpall$Plot.ID)
bpall$treatment <- gsub("W", "", bpall$treatment)
bpall$treatment <- gsub("E", "", bpall$treatment)
bpall$treatment <- gsub("HSR", "H", bpall$treatment)
bpall$treatment <- gsub("SR", "C", bpall$treatment)
bpall$treatment <- gsub('[[:digit:]]+', '', bpall$treatment)

#remove plants that have no flowers
bpsmall <- bpall[!(is.na(bpall$Flower.height)),]
bpsmall <- bpsmall[bpsmall$Flower.height != "0",]
bpsmall <- bpsmall[(!is.na(bpsmall$Leaf.height)),]  #three leaf heights were recorded as flower heights for plants with no flowers - remove
rm(bpall)
bpsmall <- bpsmall[,-c(2,4:5, 7, 11)]

bpsmall <- rename(bpsmall, plot = Plot.ID, date = Date, BD = Buds, FF = Fully.flowering, SG = Senescing, SD = Scenesced)

bpsmall <- bpsmall[,c(1,7,8,2,3,4,5,6)]

bpsmall$date <- mdy(bpsmall$date)
bpsmall$plot <- as.factor(bpsmall$plot)
bpsmall$plantid <- as.factor(bpsmall$plantid)
bpsmall$treatment <- as.factor(bpsmall$treatment)

bpsmall[is.na(bpsmall)] <- 0
bpsmall$TF <- rowSums(bpsmall[,5:8])

#To get the maximum number of flowers for plant CC6-1 in the senesced column:
#max(subset(bpsmall, plantid == "CC6-1", select = c(1:9))$SD)

cc1 <- subset(bpsmall, plantid == "CC6-1", select = c(1:9))
cc10 <- subset(bpsmall, plantid == "CC6-10", select = c(1:9))
cc4 <- subset(bpsmall, plantid == "CC6-4", select = c(1:9))
cc9 <- subset(bpsmall, plantid == "CC6-9", select = c(1:9))

ch2 <- subset(bpsmall, plantid == "CH5-2", select = c(1:9))
ch3 <- subset(bpsmall, plantid == "CH5-3", select = c(1:9))
ch4 <- subset(bpsmall, plantid == "CH5-4", select = c(1:9))
ch5 <- subset(bpsmall, plantid == "CH5-5", select = c(1:9))
ch6 <- subset(bpsmall, plantid == "CH5-6", select = c(1:9))
ch7 <- subset(bpsmall, plantid == "CH5-7", select = c(1:9))

chsr1 <- subset(bpsmall, plantid == "CHSR8-1", select = c(1:9))
chsr2 <- subset(bpsmall, plantid == "CHSR8-2", select = c(1:9))
chsr4 <- subset(bpsmall, plantid == "CHSR8-4", select = c(1:9))
chsr5 <- subset(bpsmall, plantid == "CHSR8-5", select = c(1:9))
chsr6 <- subset(bpsmall, plantid == "CHSR8-6", select = c(1:9))

csr1 <- subset(bpsmall, plantid == "CSR7-1", select = c(1:9))
csr3 <- subset(bpsmall, plantid == "CSR7-3", select = c(1:9))
csr5 <- subset(bpsmall, plantid == "CSR7-5", select = c(1:9))
csr6 <- subset(bpsmall, plantid == "CSR7-6", select = c(1:9))
csr8 <- subset(bpsmall, plantid == "CSR7-8", select = c(1:9))
csr9 <- subset(bpsmall, plantid == "CSR7-9", select = c(1:9))

ec1 <- subset(bpsmall, plantid == "EC3-1", select = c(1:9))
ec3 <- subset(bpsmall, plantid == "EC3-3", select = c(1:9))

eh11 <- subset(bpsmall, plantid == "EH4-11", select = c(1:9))
eh3 <- subset(bpsmall, plantid == "EH4-3", select = c(1:9))
eh4 <- subset(bpsmall, plantid == "EH4-4", select = c(1:9))
eh5 <- subset(bpsmall, plantid == "EH4-5", select = c(1:9))
eh6 <- subset(bpsmall, plantid == "EH4-6", select = c(1:9))
eh7 <- subset(bpsmall, plantid == "EH4-7", select = c(1:9))
eh8 <- subset(bpsmall, plantid == "EH4-8", select = c(1:9))
eh9 <- subset(bpsmall, plantid == "EH4-9", select = c(1:9))

ehsr1 <- subset(bpsmall, plantid == "EHSR1-1", select = c(1:9))
ehsr2 <- subset(bpsmall, plantid == "EHSR1-2", select = c(1:9))
ehsr4 <- subset(bpsmall, plantid == "EHSR1-4", select = c(1:9))
ehsr5 <- subset(bpsmall, plantid == "EHSR1-5", select = c(1:9))

esr1 <- subset(bpsmall, plantid == "ESR2-1", select = c(1:9))
esr11 <- subset(bpsmall, plantid == "ESR2-11", select = c(1:9))
esr2 <- subset(bpsmall, plantid == "ESR2-2", select = c(1:9))
esr3 <- subset(bpsmall, plantid == "ESR2-3", select = c(1:9))
esr4 <- subset(bpsmall, plantid == "ESR2-4", select = c(1:9))
esr6 <- subset(bpsmall, plantid == "ESR2-6", select = c(1:9))
esr8 <- subset(bpsmall, plantid == "ESR2-8", select = c(1:9))

wc1 <- subset(bpsmall, plantid == "WC11-1", select = c(1:9))
wc2 <- subset(bpsmall, plantid == "WC11-2", select = c(1:9))
wc3 <- subset(bpsmall, plantid == "WC11-3", select = c(1:9))
wc4 <- subset(bpsmall, plantid == "WC11-4", select = c(1:9))
wc5 <- subset(bpsmall, plantid == "WC11-5", select = c(1:9))
wc7 <- subset(bpsmall, plantid == "WC11-7", select = c(1:9))

whsr1 <- subset(bpsmall, plantid == "WHSR9-1", select = c(1:9))
whsr2 <- subset(bpsmall, plantid == "WHSR9-2", select = c(1:9))
whsr3 <- subset(bpsmall, plantid == "WHSR9-3", select = c(1:9))
whsr4 <- subset(bpsmall, plantid == "WHSR9-4", select = c(1:9))

wsr1 <- subset(bpsmall, plantid == "WSR10-1", select = c(1:9))
wsr3 <- subset(bpsmall, plantid == "WSR10-3", select = c(1:9))
wsr7 <- subset(bpsmall, plantid == "WSR10-7", select = c(1:9))
wsr9 <- subset(bpsmall, plantid == "WSR10-9", select = c(1:9))

library(lubridate)

balsam15 <- read.csv("nectar analysis/data files/balsam15.csv", header = T, as.is = T)
balsam16 <- read.csv("nectar analysis/data files/balsam16.csv", header = T, as.is = T)
buckwt15 <- read.csv("nectar analysis/data files/buckwt15.csv", header = T, as.is = T)
buckwt16 <- read.csv("nectar analysis/data files/buckwt16.csv", header = T, as.is = T)

balsam <- rbind(balsam15, balsam16)
buckwt <- rbind(buckwt15, buckwt16)

rm(balsam15)
rm(balsam16)
rm(buckwt15)
rm(buckwt16)

balsam$date <- ymd(balsam$date)
balsam$plot <- as.factor(balsam$plot)
balsam$treatment <- as.factor(balsam$treatment)
balsam$plant <- as.factor(balsam$plant)

buckwt$date <- ymd(buckwt$date)
buckwt$plot <- as.factor(buckwt$plot)
buckwt$treatment <- as.factor(buckwt$treatment)
buckwt$quad <- as.factor(buckwt$quad)

# Calculate percentage of flowers that aren't producing nectar

balscontrol <- subset(balsam, treatment == "C")
pz.bac <- sum(balscontrol$volume == "0")/nrow(balscontrol)
pnz.bac <- sum(balscontrol$volume != "0")/nrow(balscontrol)

balsheat <- subset(balsam, treatment == "H")
pz.bah <- sum(balsheat$volume == "0")/nrow(balsheat)
pnz.bah <- sum(balsheat$volume != "0")/nrow(balsheat)

buckcontrol <- subset(buckwt, treatment == "C")
pz.buc <- sum(buckcontrol$volume == "0")/nrow(buckcontrol)
pnz.buc <- sum(buckcontrol$volume != "0")/nrow(buckcontrol)

buckheat <- subset(buckwt, treatment == "H")
pz.buh <- sum(buckheat$volume == "0")/nrow(buckheat)
pnz.buh <- sum(buckheat$volume != "0")/nrow(buckheat)

balscontrol <- balscontrol[,-c(6,7)]
balsheat <- balsheat[,-c(6,7)]
buckcontrol <- buckcontrol[,-c(6,7)]
buckheat <- buckheat[,-c(6,7)]

balscontrol$volume[balscontrol$volume != "0"] <- "1"
balscontrol$necpres<- as.factor(balscontrol$volume)
balscontrol <- balscontrol[,-5]

balsheat$volume[balsheat$volume != "0"] <- "1"
balsheat$necpres<- as.factor(balsheat$volume) 
balsheat <- balsheat[,-5]

buckcontrol$volume[buckcontrol$volume != "0"] <- "1"
buckcontrol$necpres<- as.factor(buckcontrol$volume)
buckcontrol <- buckcontrol[,-5]

buckheat$volume[buckheat$volume != "0"] <- "1"
buckheat$necpres<- as.factor(buckheat$volume) 
buckheat <- buckheat[,-5]

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

bucksheat <- subset(buckwt, treatment == "H")
pz.buh <- sum(bucksheat$volume == "0")/nrow(bucksheat)
pnz.buh <- sum(bucksheat$volume != "0")/nrow(bucksheat)




library(lubridate)

# Read and minor munging of dataframes

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
balsam$year <- as.factor(year((balsam$date)))
                         

buckwt$date <- ymd(buckwt$date)
buckwt$plot <- as.factor(buckwt$plot)
buckwt$treatment <- as.factor(buckwt$treatment)
buckwt$quad <- as.factor(buckwt$quad)
buckwt$year <- as.factor(year((buckwt$date)))

# Calculate percentage of flowers that are/aren't producing nectar

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

rm(balscontrol)
rm(balsheat)
rm(buckcontrol)
rm(buckheat)

# Create dataframes for linear analysis

balsam$necpres[balsam$volume != "0"] <- "1"
balsam$necpres[balsam$volume == "0"] <- "0"
balsam$necpres <- as.factor(balsam$necpres)
balsam <- balsam[,-c(5:7)]

buckwt$necpres[buckwt$volume != "0"] <- "1"
buckwt$necpres[buckwt$volume == "0"] <- "0"
buckwt$necpres <- as.factor(buckwt$necpres)
buckwt <- buckwt[,-c(5:7)]

# Models

modbals <- glmer(necpres ~ treatment * year + (1|plot/plant) + (1| year:date), data = balsam, family = binomial)
summary(modbals)

modbuck <- glmer(necpres ~ treatment * year + (1|plot) + (1| year:date), data = buckwt, family = binomial)  #still need to add quadrant random effect +(1|plot/quad)
summary(modbuck)



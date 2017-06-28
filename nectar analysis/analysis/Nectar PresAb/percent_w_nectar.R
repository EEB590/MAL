library(lubridate)
library(lme4)
library(nlme)
library(lsmeans)
library(car)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

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

# Create dataframes for regression analysis

balsam$necpres[balsam$volume != "0"] <- "1"
balsam$necpres[balsam$volume == "0"] <- "0"
balsam$necpres <- as.factor(balsam$necpres)
balsam <- balsam[,-c(5:7)]

buckwt$necpres[buckwt$volume != "0"] <- "1"
buckwt$necpres[buckwt$volume == "0"] <- "0"
buckwt$necpres <- as.factor(buckwt$necpres)
buckwt <- buckwt[,-c(5:7)]

#Data exploration
with(balsam, plot(as.factor(date), necpres, main = "Balsamroot", xlab = "Date", ylab = "Absent (0:dark) / Present (1:light)"))
with(buckwt, plot(as.factor(date), necpres, main = "Buckwheat", xlab = "Date", ylab = "Absent (0:dark) / Present (1:light)"))

with(balsam, plot(plot, necpres, main = "Balsamroot", xlab = "Treatment Plot", ylab = "Absent (0:dark) / Present (1:light)"))
with(buckwt, plot(plot, necpres, main = "Buckwheat", xlab = "Treatment Plot", ylab = "Absent (0:dark) / Present (1:light)"))

#Remove 2015 buckwheat from analysis (because all 1's)
buckwt <- subset(buckwt, year != "2015", select = date:necpres)

# Models
  #balsamroot
modbals <- glmer(necpres ~ treatment * year + (1|plot/plant) + (1| year:date), data = balsam, family = binomial)
summary(modbals)
  #interaction?
Anova(modbals, type = 3)

cellN <- with(balsam, table(treatment, year))
cellN

necpres.grid <- ref.grid(modbals)
int.necpres <- pairs(necpres.grid, by = "year")
int.necprestable <- update(int.necpres, by = NULL)
int.necprestable

summary(necpres.grid)

lsmeans(necpres.grid, "treatment")
lsmeans(necpres.grid, "year")

necpres.treat <- lsmeans(necpres.grid, "treatment")
pairs(necpres.treat)

necpres.year <- lsmeans(necpres.grid, "year")
pairs(necpres.year)


  #buckwheat (only 2016)
modbuck <- glmer(necpres ~ treatment + (1|plot) + (1|date), data = buckwt, family = binomial)
summary(modbuck)

Anova(modbuck)

cellN <- with(buckwt, table(treatment, year))
cellN

necpres.grid.buck <- ref.grid(modbuck)
summary(necpres.grid.buck)

lsmeans(necpres.grid.buck, "treatment")

necpres.treat <- lsmeans(necpres.grid.buck, "treatment")
pairs(necpres.treat)


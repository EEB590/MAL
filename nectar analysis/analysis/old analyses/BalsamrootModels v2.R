library(ggplot2)
library(nlme)
library(car)
library(visreg)

######################
### 2015-2016 data ###
######################

# Create the  data frames

##Read in the data

setwd("D:/Iowa State University/Debinski Lab/Nectar data/Nectar analysis for manuscript")

  # Balsamroot volume, 2015-2016

volume.balsam <- read.csv("Nectar_Vol_Balsam.csv", header = T, col.names = c("Date", "Date.Factor", "Year.Factor", "Plot", "Heat", "Plant", "Volume"))
volume.balsam <- data.frame(volume.balsam[,1:7])
volume.balsam$Date.Factor <- as.factor(volume.balsam$Date.Factor)
volume.balsam$Year.Factor <- as.factor(volume.balsam$Year.Factor) # 1 = 2015, 2 = 2016
volume.balsam$Heat <- as.factor(volume.balsam$Heat) # 0 = control, 1 = heat
volume.balsam$Plant <- as.factor(paste(volume.balsam$Plot,volume.balsam$Plant,sep="-"))
volume.balsam$Plot <- as.factor(volume.balsam$Plot)
volume.balsam$log.vol <- log(volume.balsam$Volume)
head(volume.balsam)

  # Balsamroot sugar, 2015-2016

sugar.balsam <- read.csv("Nectar_BRIX_Balsam.csv", header = T, col.names = c("Date", "Date.Factor", "Year.Factor", "Plot", "Heat","Plant", "BRIX", "Mass"))
sugar.balsam <- data.frame(sugar.balsam[,1:8])
sugar.balsam$Date.Factor <- as.factor(sugar.balsam$Date.Factor)
sugar.balsam$Year.Factor <- as.factor(sugar.balsam$Year.Factor)
sugar.balsam$Heat <- as.factor(sugar.balsam$Heat)
sugar.balsam$Plant <- as.factor(paste(sugar.balsam$Plot,sugar.balsam$Plant,sep="-"))
sugar.balsam$Plot <- as.factor(sugar.balsam$Plot)
sugar.balsam$log.mass <- log(sugar.balsam$Mass)
head(sugar.balsam)


# Models

vol.balsam.mod <- lme(log.vol ~ Heat, random = ~1 | Plant, data = volume.balsam)
vol.balsam.mod
summary(vol.balsam.mod)
plot(vol.balsam.mod, main = "Balsamroot Volume, 2015-2016")

conc.balsam.mod <- lme(BRIX ~ Heat, random = ~1 | Plant, data = sugar.balsam)
conc.balsam.mod
summary(conc.balsam.mod)
plot(conc.balsam.mod, main = "Balsamroot Concentration, 2015-2016")

mass.balsam.mod <- lme(log.mass ~ Heat, random = ~1 | Plant, data = sugar.balsam)
mass.balsam.mod
summary(mass.balsam.mod)
plot(mass.balsam.mod, main = "Balsamroot Sugar Mass, 2015-2016")

#######################
###  2016 only data ###
#######################

volume.balsam2016 <- as.data.frame(volume.balsam[volume.balsam$Year.Factor == "2",])
sugar.balsam2016 <- as.data.frame((sugar.balsam[sugar.balsam$Year.Factor == "2",]))

vol.2016.mod <- lme(log.vol ~ Heat, random = ~1 | Plant, data = volume.balsam2016)
vol.2016.mod
summary(vol.2016.mod)
plot(vol.2016.mod, main = "Balsamroot Volume (log transformed), 2016 only")

conc.2016.mod <- lme(BRIX ~ Heat, random = ~1 | Plant, data = sugar.balsam2016)
conc.2016.mod
summary(conc.2016.mod)
plot(conc.2016.mod, main = "Balsamroot Concentration, 2016 only")

mass.2016.mod <- lme(log.mass ~ Heat, random = ~1 | Plant, data = sugar.balsam2016)
mass.2016.mod
summary(mass.2016.mod)
plot(mass.2016.mod, main = "Balsamroot Mass (log transformed), 2016 only")

#######################
###  2015 only data ###
#######################

volume.balsam2015 <- as.data.frame(volume.balsam[volume.balsam$Year.Factor == "1",])
sugar.balsam2015 <- as.data.frame((sugar.balsam[sugar.balsam$Year.Factor == "1",]))

vol.2015.mod <- lme(log.vol ~ Heat, random = ~1 | Plant, data = volume.balsam2015)
visreg(vol.2015.mod, points.par = list(cex = 1.2, col = "black"))
vol.2015.mod
Anova(vol.2015.mod, type = 3)  #p-value = 0.06678
plot(vol.2015.mod, main = "Balsamroot Volume (log transformed), 2015 only")

conc.2015.mod <- lme(BRIX ~ Heat, random = ~1 | Plant, data = sugar.balsam2015)
visreg(conc.2015.mod, points.par = list(cex = 1.2, col = "black"))
conc.2015.mod
Anova(conc.2015.mod, type = 3) #p-value 0.8601
plot(conc.2015.mod, main = "Balsamroot Concentration, 2015 only")

mass.2015.mod <- lme(log.mass ~ Heat, random = ~1 | Plant, data = sugar.balsam2015)
visreg(mass.2015.mod, points.par = list(cex = 1.2, col = "black"))
mass.2015.mod
Anova(mass.2015.mod, type = 3) #p-value = 0.6347
plot(mass.2015.mod, main = "Balsamroot Mass (log transformed), 2015 only")



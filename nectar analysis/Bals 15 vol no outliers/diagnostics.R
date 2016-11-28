library(ggplot2)
library(lme4)
library(nlme)
library(lubridate)
library(influence.ME)

#setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL") only need this to knit

balsvol15 <- read.csv("nectar analysis/Bals 15 vol no outliers/balsvol15sub.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)
balsvolboth <- rbind(balsvol15,balsvol16)
balsvolboth$year <- as.factor(year(balsvolboth$date))

balssug15 <- read.csv("nectar analysis/data files/balssugar15.csv", header = T)
balssug16 <- read.csv("nectar analysis/data files/balssugar16.csv", header = T)
balssugboth <- rbind(balssug15,balssug16)
balssugboth$year <- as.factor(year(balssugboth$date))

modvol <- lmer(volume ~ treatment * year + (1|plant), data = balsvolboth)
plot(modvol, main = "Balsam Volume")
inflvol <- influence(modvol, obs = T)
plot(inflvol, which = "cook", main = "Balsam Volume")

modBRIX <- lmer(BRIX ~ treatment * year + (1|plant), data = balssugboth)
plot(modBRIX, main = "Balsam BRIX")
inflBRIX <- influence(modBRIX, obs = T)
plot(inflBRIX, which = "cook", main = "Balsam BRIX")

modmass <- lmer(mass ~ treatment * year + (1|plant), data = balssugboth)
plot(modmass, main = "Balsam mass")
inflmass <- influence(modmass, obs = T)
plot(inflmass, which = "cook", main = "Balsam mass")

library(ggplot2)
library(lme4)
library(nlme)
library(lubridate)
library(influence.ME)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

buckvol15 <- read.csv("nectar analysis/data files/buckvol15.csv", header = T)
buckvol16 <- read.csv("nectar analysis/data files/buckvol16.csv", header = T)
buckvolboth <- rbind(buckvol15,buckvol16)
buckvolboth$year <- as.factor(year(buckvolboth$date))

bucksug15 <- read.csv("nectar analysis/data files/bucksugar15.csv", header = T)
bucksug16 <- read.csv("nectar analysis/data files/bucksugar16.csv", header = T)
bucksugboth <- rbind(bucksug15,bucksug16)
bucksugboth$year <- as.factor(year(bucksugboth$date))

modvol <- lmer(volume ~ treatment * year + (1|plot), data = buckvolboth)
plot(modvol, main = "Buckwheat volume")
inflvol <- influence(modvol, obs = T)
plot(inflvol, which = "cook", main = "Buckwheat volume")

modBRIX <- lmer(BRIX ~ treatment * year + (1|plot), data = bucksugboth)
plot(modBRIX, main = "Buckwheat BRIX")
inflBRIX <- influence(modBRIX, obs = T)
plot(inflBRIX, which = "cook", main = "Buckwheat BRIX")

modmass <- lmer(mass ~ treatment * year + (1|plot), data = bucksugboth)
plot(modmass, main = "Buckwheat mass")
inflmass <- influence(modmass, obs = T)
plot(inflmass, which = "cook", main = "Buckwheat mass")

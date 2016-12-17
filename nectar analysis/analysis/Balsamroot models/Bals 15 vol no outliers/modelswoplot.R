library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

balsvol15 <- read.csv("nectar analysis/Bals 15 vol no outliers/balsvol15sub.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)
balsvolboth <- rbind(balsvol15,balsvol16)

balsvolboth$year <- as.factor(year(balsvolboth$date))

cellN <- with(balsvolboth, table(treatment, year))
cellN

cellMean <- with(balsvolboth, tapply(volume, list(treatment, year), mean))
cellMean

modvol <- lmer(volume ~ treatment * year + (1|plant), data = balsvolboth)

volume.grid <- ref.grid(modvol)
summary(volume.grid)

lsmeans(volume.grid, "treatment")
lsmeans(volume.grid, "year")

volume.treat <- lsmeans(volume.grid, "treatment")
pairs(volume.treat)
pairs.treat <- pairs(volume.treat)
test(pairs.treat, joint = T)

volume.year <- lsmeans(volume.grid, "year")
pairs(volume.year)
pairs.year <- pairs(volume.year)
test(pairs.year, joint = T)

int.vol <- pairs(volume.grid, by = "year")
int.vol
int.voltable <- update(int.vol, by = NULL)
int.voltable

test(pairs(int.voltable), joint = T)

Anova(modvol, type = 3)


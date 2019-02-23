library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

buckvol15 <- read.csv("nectar analysis/data files/buckvol15.csv", header = T)
buckvol16 <- read.csv("nectar analysis/data files/buckvol16.csv", header = T)
buckvolboth <- rbind(buckvol15,buckvol16)
rm(buckvol16)
rm(buckvol15)

buckvolboth$lnvol <- log(buckvolboth$volume)
buckvolboth$year <- as.factor(year(buckvolboth$date))

cellN <- with(buckvolboth, table(treatment, year))
cellN

cellMean <- with(buckvolboth, tapply(volume, list(treatment, year), mean))
cellMean

modlnvol <- lmer(lnvol ~ treatment * year + (1|plot) +(1|year:date), data = buckvolboth)
summary(modlnvol)

buck.vol <- emmeans::emmeans(modlnvol, c("treatment", "year"), type='response')
emmeans::joint_tests(buck.vol)

plot(modlnvol)
inflvol <- influence(modlnvol, obs = T)
plot(inflvol, which = "cook", main = "Buckwheat volume")

lnvol.grid <- ref.grid(modlnvol)
summary(lnvol.grid)

lnvol.treat <- lsmeans(lnvol.grid, "treatment")
pairs(lnvol.treat)

lnvol.year <- lsmeans(lnvol.grid, "year")
pairs(lnvol.year)

int.vol <- pairs(lnvol.grid, by = "year")
int.voltable <- update(int.vol, by = NULL)
int.voltable

test(pairs(int.voltable), joint = T)

Anova(modlnvol, type = 3)


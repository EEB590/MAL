library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

balssug15 <- read.csv("nectar analysis/data files/balssugar15.csv", header = T)
balssug16 <- read.csv("nectar analysis/data files/balssugar16.csv", header = T)
balssugboth <- rbind(balssug15,balssug16)
rm(balssug15)
rm(balssug16)

balssugboth$year <- as.factor(year(balssugboth$date))

cellN <- with(balssugboth, table(treatment, year))
cellN

cellMean <- with(balssugboth, tapply(BRIX, list(treatment, year), mean))
cellMean

modBRIX <- lmer(BRIX ~ treatment * year +(1|plot/plant) + (1|year:date), data = balssugboth)
summary(modBRIX)
plot(modBRIX, main = "Balsam BRIX")
inflBRIX <- influence(modBRIX, obs = T)
plot(inflBRIX, which = "cook", main = "Balsam BRIX")

BRIX.grid <- ref.grid(modBRIX)
summary(BRIX.grid)

lsmeans(BRIX.grid, "treatment")
lsmeans(BRIX.grid, "year")

BRIX.treat <- lsmeans(BRIX.grid, "treatment")
pairs(BRIX.treat)

BRIX.year <- lsmeans(BRIX.grid, "year")
pairs(BRIX.year)

int.BRIX <- pairs(BRIX.grid, by = "year")
int.BRIXtable <- update(int.BRIX, by = NULL)
int.BRIXtable

test(pairs(int.BRIXtable), joint = T)

Anova(modBRIX, type = 3)


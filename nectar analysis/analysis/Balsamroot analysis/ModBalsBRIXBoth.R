library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

#setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL") only need this to knit

balssug15 <- read.csv("nectar analysis/data files/balssugar15.csv", header = T)
balssug16 <- read.csv("nectar analysis/data files/balssugar16.csv", header = T)
balssugboth <- rbind(balssug15,balssug16)

balssugboth$year <- as.factor(year(balssugboth$date))

cellN <- with(balssugboth, table(treatment, year))
cellN

cellMean <- with(balssugboth, tapply(BRIX, list(treatment, year), mean))
cellMean

modBRIX <- lmer(BRIX ~ treatment * year + (1|plant), data = balssugboth)

BRIX.grid <- ref.grid(modBRIX)
summary(BRIX.grid)

lsmeans(BRIX.grid, "treatment")
lsmeans(BRIX.grid, "year")

BRIX.treat <- lsmeans(BRIX.grid, "treatment")
pairs(BRIX.treat)
pairs.treat <- pairs(BRIX.treat)
test(pairs.treat, joint = T)

BRIX.year <- lsmeans(BRIX.grid, "year")
pairs(BRIX.year)
pairs.year <- pairs(BRIX.year)
test(pairs.year, joint = T)

int.BRIX <- pairs(BRIX.grid, by = "year")
int.BRIX
int.BRIXtable <- update(int.BRIX, by = NULL)
int.BRIXtable

test(pairs(int.BRIXtable), joint = T)

Anova(modBRIX, type = 3)


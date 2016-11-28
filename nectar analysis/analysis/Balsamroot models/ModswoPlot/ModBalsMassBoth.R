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

balssugboth$year <- as.factor(year(balssugboth$date))

cellN <- with(balssugboth, table(treatment, year))
cellN

cellMean <- with(balssugboth, tapply(mass, list(treatment, year), mean))
cellMean

modmass <- lmer(mass ~ treatment * year + (1|plant), data = balssugboth)

mass.grid <- ref.grid(modmass)
summary(mass.grid)

lsmeans(mass.grid, "treatment")
lsmeans(mass.grid, "year")

mass.treat <- lsmeans(mass.grid, "treatment")
pairs(mass.treat)
pairs.treat <- pairs(mass.treat)
test(pairs.treat, joint = T)

mass.year <- lsmeans(mass.grid, "year")
pairs(mass.year)
pairs.year <- pairs(mass.year)
test(pairs.year, joint = T)

int.mass <- pairs(mass.grid, by = "year")
int.mass
int.masstable <- update(int.mass, by = NULL)
int.masstable

test(pairs(int.masstable), joint = T)

Anova(modmass, type = 3)


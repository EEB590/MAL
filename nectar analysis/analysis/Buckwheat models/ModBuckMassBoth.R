library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

bucksug15 <- read.csv("nectar analysis/data files/bucksugar15.csv", header = T)
bucksug16 <- read.csv("nectar analysis/data files/bucksugar16.csv", header = T)
bucksugboth <- rbind(bucksug15,bucksug16)
rm(bucksug15)
rm(bucksug16)

bucksugboth$lnmass <- log(bucksugboth$mass)
bucksugboth$year <- as.factor(year(bucksugboth$date))

cellN <- with(bucksugboth, table(treatment, year))
cellN

cellMean <- with(bucksugboth, tapply(mass, list(treatment, year), mean))
cellMean

modlnmass <- lmer(lnmass ~ treatment * year + (1|plot) + (1|year:date), data = bucksugboth)
summary(modlnmass)
plot(modlnmass)
inflmass <- influence(modlnmass, obs = T)
plot(inflmass, which = "cook", main = "Buckwheat mass")

mass.grid <- ref.grid(modlnmass)
summary(mass.grid)

mass.treat <- lsmeans(mass.grid, "treatment")
pairs(mass.treat)

mass.year <- lsmeans(mass.grid, "year")
pairs(mass.year)

int.mass <- pairs(mass.grid, by = "year")
int.masstable <- update(int.mass, by = NULL)
int.masstable

test(pairs(int.masstable), joint = T)

Anova(modlnmass, type = 3)


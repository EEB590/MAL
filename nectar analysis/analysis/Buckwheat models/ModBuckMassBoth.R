library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

# setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL") only need this to knit

bucksug15 <- read.csv("nectar analysis/data files/bucksugar15.csv", header = T)
bucksug16 <- read.csv("nectar analysis/data files/bucksugar16.csv", header = T)
bucksugboth <- rbind(bucksug15,bucksug16)

bucksugboth$year <- as.factor(year(bucksugboth$date))

cellN <- with(bucksugboth, table(treatment, year))
cellN

cellMean <- with(bucksugboth, tapply(mass, list(treatment, year), mean))
cellMean

modmass <- lmer(mass ~ treatment * year + (1|plot), data = bucksugboth)

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


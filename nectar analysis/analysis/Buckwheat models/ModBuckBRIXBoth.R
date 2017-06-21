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

bucksugboth$year <- as.factor(year(bucksugboth$date))

cellN <- with(bucksugboth, table(treatment, year))
cellN

cellMean <- with(bucksugboth, tapply(BRIX, list(treatment, year), mean))
cellMean

# modBRIX.plot <- lmer(BRIX ~ treatment * year + (1|plot/quad), data = bucksugboth)
# ran this model on Adam's suggestion, but there's almost no difference between this and the original model (without quad random effect). Estimates are within 0.01 and 0.05 for C and H, respectively, and p-value goes from 0.0192 (no quad) to 0.0231 with quadrant random effect.  Exploratory plots also show very little variability among quadrants.

modBRIX <- lmer(BRIX ~ treatment * year + (1|plot) + (1|year:date), data = bucksugboth)  
summary(modBRIX)

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


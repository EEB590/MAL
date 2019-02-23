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

balssugboth$lnmass <- log(balssugboth$mass)
balssugboth$year <- as.factor(year(balssugboth$date))

cellN <- with(balssugboth, table(treatment, year))
cellN

cellMean <- with(balssugboth, tapply(mass, list(treatment, year), mean))
cellMean

modlnmass <- lmer(lnmass ~ treatment * year + (1|plot/plant) + (1|year:date), data = balssugboth)
summary(modlnmass)

bals.mass <- emmeans::emmeans(modlnmass, c("treatment", "year"), type='response')
emmeans::joint_tests(bals.mass)

plot(modlnmass)
inflmass <- influence(modlnmass, obs = T)
plot(inflmass, which = "cook", main = "Balsam mass")

lnmass.grid <- ref.grid(modlnmass)
summary(lnmass.grid)

mass.treat <- lsmeans(lnmass.grid, "treatment")
pairs(mass.treat)

mass.year <- lsmeans(lnmass.grid, "year")
pairs(mass.year)

int.mass <- pairs(lnmass.grid, by = "year")
int.masstable <- update(int.mass, by = NULL)
int.masstable

test(pairs(int.masstable), joint = T)

Anova(modlnmass, type = 3)


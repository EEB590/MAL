library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

balsvol15 <- read.csv("nectar analysis/data files/balsvol15.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)

modvol15 <- lmer(volume ~ treatment + (1|plant), data = balsvol15)
Anova(modvol15, type = 3)

modvol16 <- lmer(volume ~ treatment + (1|plant), data = balsvol16)
Anova(modvol16, type = 3)

balssug15 <- read.csv("nectar analysis/data files/balssugar15.csv", header = T)
balssug16 <- read.csv("nectar analysis/data files/balssugar16.csv", header = T)

modBRIX15 <- lmer(BRIX ~ treatment + (1|plant), data = balssug15)
Anova(modBRIX15, type = 3)

modBRIX16 <- lmer(BRIX ~ treatment + (1|plant), data = balssug16)
Anova(modBRIX16, type = 3)

modmass15 <- lmer(mass ~ treatment + (1|plant), data = balssug15)
Anova(modmass15, type = 3)

modmass16 <- lmer(mass ~ treatment + (1|plant), data = balssug16)
Anova(modmass16, type = 3)

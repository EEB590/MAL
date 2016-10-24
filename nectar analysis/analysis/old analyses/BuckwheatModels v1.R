library(ggplot2)
library(GGally)
library(dplyr)
library(mvnormtest)
library(HH)
library(Rcmdr)
library(nlme)
library(car)
library(lmerTest)

# Create the  data frames

##Read in the data

setwd("D:/Iowa State University/Debinski Lab/Nectar data/Nectar analysis for manuscript")

  # Buckwheat volume, 2015-2016

volume.buck <- read.csv("Nectar_Vol_Buck.csv", header = T)
volume.buck <- data.frame(volume.buck[,1:7])
volume.buck$Date.Factor <- as.factor(volume.buck$Date.Factor)
volume.buck$Year.Factor <- as.factor(volume.buck$Year.Factor) # 1 = 2015, 2 = 2016
volume.buck$Heat <- as.factor(volume.buck$Heat) # 0 = control, 1 = heat treatment
volume.buck$Sample.Round <- as.factor(volume.buck$Sample.Round)
volume.buck$trans.vol <- log10(volume.buck$Volume)  #this doesn't work
head(volume.buck)

  # Buckwheat sugar, 2015-2016

sugar.buck <- read.csv("Nectar_BRIX_Buck.csv", header = T, col.names = c("Date", "Date.Factor", "Year.Factor", "Plot", "Heat", "BRIX", "Mass"))
sugar.buck <- data.frame(sugar.buck[,1:7])
sugar.buck$Date.Factor <- as.factor(sugar.buck$Date.Factor)
sugar.buck$Year.Factor <- as.factor(sugar.buck$Year.Factor)
sugar.buck$Heat <- as.factor(sugar.buck$Heat)
sugar.buck$Mass <- as.numeric(sugar.buck$Mass)
sugar.buck$BRIX <- as.numeric(sugar.buck$BRIX)
sugar.buck$trans.mass <- (sugar.buck$Mass^(1/3))
sugar.buck$trans.conc <- (sugar.buck$BRIX^(2))
head(sugar.buck)

######  MODELS  ########

## VOLUME

# from last year
# volHS <- lme(Volume ~ Heat*Snow, random = ~1 | plot,weights = varIdent(form = ~1 | data_factor), data = data)

# similar code for this year
# using both years the varIdent doesn't converge

# vol.mod1 <- lme(trans.vol ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = volume.buck)

  # transformed data for both years
vol.buck.mod <- lme(trans.vol ~ Heat, random = ~1 | Plot, data = volume.buck)
vol.buck.mod
Anova(vol.buck.mod)
summary(vol.buck.mod)   #p-value = 0.0292
plot(vol.buck.mod, main = "Buckwheat Volume (transformed data)")

  # untransformed data for both years
vol.buck.mod2 <- lme(Volume ~ Heat, random = ~1 | Plot, data = volume.buck)
vol.buck.mod2
Anova(vol.buck.mod2)
summary(vol.buck.mod2)   #p-value = 0.0309
plot(vol.buck.mod2, main = "Buckwheat Volume (untransformed data")

  #transformed data 2015
volume.buck2015 <- as.data.frame(volume.buck[volume.buck$Year.Factor == "1",])
vol.mod2 <- lme(trans.vol ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = volume.buck2015)
anova(vol.mod2)
Anova(vol.mod2)
summary(vol.mod2)  #p-value = 0.0117

  #untransformed data 2015
vol.mod2a <- lme(Volume ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = volume.buck2015)
anova(vol.mod2a)
Anova(vol.mod2a)
summary(vol.mod2a)  #p-value = 0.0332

  #transformed data 2016
volume.buck2016 <- as.data.frame(volume.buck[volume.buck$Year.Factor == "2",])
vol.mod3 <- lme(trans.vol ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = volume.buck2016)
anova(vol.mod3)
Anova(vol.mod3)
summary(vol.mod3)   #p-value = 0.6111

  #untransformed data 2016
vol.mod3a <- lme(Volume ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = volume.buck2016)
anova(vol.mod3a)
Anova(vol.mod3a)
summary(vol.mod3a)   #p-value = 0.371



##  CONCENTRATION (BRIX)

#From last year
# brixHS <- lme(BRIX ~ Heat*Snow, random = ~1 | plot, weights = varIdent(form = ~1 | data_factor), data = data)

# similar code for this year
# using both years the varIdent doesn't converge (for either BRIX or mass)

# conc.mod1 <- lme(trans.conc ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck)

conc.buck.mod <- lme(trans.conc ~ Heat, random = ~1 | Plot, data = sugar.buck, na.action(na.omit))
conc.buck.mod
summary(conc.buck.mod)  #p-value = 0.0149
plot(conc.buck.mod, main = "Buckwheat Concentration (transformed data)")

conc.buck.mod2 <- lme(BRIX ~ Heat, random = ~1 | Plot, data = sugar.buck, na.action(na.omit))
conc.buck.mod2
summary(conc.buck.mod2)  #p-value = 0.0186
plot(conc.buck.mod2, main = "Buckwheat Concentration (untransformed data)")

sugar.buck2015 <- as.data.frame(sugar.buck[sugar.buck$Year.Factor == "1",])

conc.mod2 <- lme(trans.conc ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
anova(conc.mod2)
summary(conc.mod2)  #p-value = 0.0014

conc.mod2a <- lme(BRIX ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
anova(conc.mod2a)
summary(conc.mod2a)  #p-value = 0.0014

sugar.buck2016 <- as.data.frame(sugar.buck[sugar.buck$Year.Factor == "2",])

conc.mod3 <- lme(trans.conc ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2016)
anova(conc.mod3)
summary(conc.mod3)  # p-value = 0.347

conc.mod3a <- lme(BRIX ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2016)
anova(conc.mod3a)
summary(conc.mod3a)   #p-value = 0.3299


## MASS

# mass.mod1 <- lme(trans.mass ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck)

mass.buck.mod <- lme(trans.mass ~ Heat, random = ~1 | Plot, data = sugar.buck)
mass.buck.mod
summary(mass.buck.mod)  #p-value = 0.20
plot(mass.buck.mod, main = "Buckwheat Mass (transformed data)")

mass.buck.mod2 <- lme(Mass ~ Heat, random = ~1 | Plot, data = sugar.buck)
mass.buck.mod2
summary(mass.buck.mod2)  #p-value = 0.02016
plot(mass.buck.mod2, main = "Buckwheat Mass (untransformed data)")


mass.mod2 <- lme(trans.mass ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
anova(mass.mod2)
summary(mass.mod2) #p-value = 0.19

mass.mod2a <- lme(Mass ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
anova(mass.mod2a)
summary(mass.mod2a)  #p-value = 0.1629


mass.mod3 <- lme(trans.mass ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2016)
anova(mass.mod3)
summary(mass.mod3)  #p-value = 0.3117

mass.mod3a <- lme(Mass ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2016)
anova(mass.mod3a)
summary(mass.mod3a)  #p-value = 0.2799


library(ggplot2)
library(car)
library(nlme)
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

  #transformed data 2015
volume.buck2015 <- as.data.frame(volume.buck[volume.buck$Year.Factor == "1",])
vol.mod2 <- lme(trans.vol ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = volume.buck2015)
visreg(vol.mod2, points.par = list(cex = 1.2, col = "black"))
Anova(vol.mod2, type = 3)  #p-value = 0.002089
plot(vol.mod2)

  #untransformed data 2015
vol.mod2a <- lme(Volume ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = volume.buck2015)
visreg(vol.mod2a, points.par = list(cex = 1.2, col = "black"))
Anova(vol.mod2a, type = 3)  #p-value = 0.01356
plot(vol.mod2a)


##  CONCENTRATION (BRIX)

sugar.buck2015 <- as.data.frame(sugar.buck[sugar.buck$Year.Factor == "1",])

  #transformed data
conc.mod2 <- lme(trans.conc ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
visreg(conc.mod2, points.par = list(cex = 1.2, col = "black"))
Anova(conc.mod2, type = 3)  #p-value = 1.22e-05
plot(conc.mod2)

  #untransformed data
conc.mod2a <- lme(BRIX ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
visreg(conc.mod2a, points.par = list(cex = 1.2, col = "black"))
Anova(conc.mod2a, type = 3)  #p-value = 1.352e-05
plot(conc.mod2a)

## MASS

  #transformed
mass.mod2 <- lme(trans.mass ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
visreg(mass.mod2, points.par = list(cex = 1.2, col = "black"))
Anova(mass.mod2, type = 3)  #p-value = 0.1597
plot(mass.mod2)

  #untransformed
mass.mod2a <- lme(Mass ~ Heat, random = ~1 | Plot, weights = varIdent(form = ~1 | Date.Factor), data = sugar.buck2015)
visreg(mass.mod2a, points.par = list(cex = 1.2, col = "black"))
Anova(mass.mod2a, type = 3)  #p-value = 0.132
plot(mass.mod2a)

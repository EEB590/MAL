library(ggplot2)
library(dplyr)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

bucksug15 <- read.csv("nectar analysis/data files/bucksugar15.csv", header = T)
bucksug16 <- read.csv("nectar analysis/data files/bucksugar16.csv", header = T)
bucksugboth <- rbind(bucksug15,bucksug16)

bucksug15$lnmass <- log(bucksug15$mass)
bucksug16$lnmass <- log(bucksug16$mass)
bucksugboth$lnmass <- log(bucksugboth$mass)

#Data summaries

summary(bucksug15)
summary(bucksug16)
summary(bucksugboth)

summarize(group_by(bucksug15, treatment), meanBRIX = mean(BRIX), sdBRIX = sd(BRIX))
summarize(group_by(bucksug16, treatment), meanBRIX = mean(BRIX), sdBRIX = sd(BRIX))
summarize(group_by(bucksugboth, treatment), meanBRIX = mean(BRIX), sdBRIX = sd(BRIX))

summarize(group_by(bucksug15, treatment), meanmass = mean(mass), sdmass = sd(mass))
summarize(group_by(bucksug16, treatment), meanmass = mean(mass), sdmass = sd(mass))
summarize(group_by(bucksugboth, treatment), meanmass = mean(mass), sdmass = sd(mass))

qplot(bucksug15$BRIX, binwidth = 1)
qplot(bucksug16$BRIX, binwidth = 1)
qplot(bucksugboth$BRIX, binwidth = 1)

qplot(bucksug15$mass, binwidth = 0.005)
qplot(bucksug16$mass, binwidth = .0025)
qplot(bucksugboth$mass, binwidth = .0025)

qplot(bucksug15$lnmass, binwidth = 0.05)
qplot(bucksug16$lnmass, binwidth = .05)
qplot(bucksugboth$lnmass, binwidth = .05)


ggplot(bucksug15, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Buckwheat BRIX 2015")

ggplot(bucksug16, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Buckwheat BRIX 2016")

ggplot(bucksugboth, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Buckwheat BRIX 2015 & 2016")

ggplot(bucksug15, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass") + ggtitle("Buckwheat sugar mass 2015")

ggplot(bucksug16, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass") + ggtitle("Buckwheat sugar mass 2016")

ggplot(bucksugboth, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass") + ggtitle("Buckwheat sugar mass 2015 & 2016")

# Homoscedastic?

var15C <- sd(bucksug15$BRIX[bucksug15$treatment=="C"])^2
var15H <- sd(bucksug15$BRIX[bucksug15$treatment=="H"])^2
ratio15 <- var15C/var15H
ratio15

var16C <- sd(bucksug16$BRIX[bucksug16$treatment=="C"])^2
var16H <- sd(bucksug16$BRIX[bucksug16$treatment=="H"])^2
ratio16 <- var16C/var16H
ratio16

varbothC <- sd(bucksugboth$BRIX[bucksugboth$treatment=="C"])^2
varbothH <- sd(bucksugboth$BRIX[bucksugboth$treatment=="H"])^2
ratioboth <- varbothC/varbothH
ratioboth

var15C <- sd(bucksug15$mass[bucksug15$treatment=="C"])^2
var15H <- sd(bucksug15$mass[bucksug15$treatment=="H"])^2
ratio15 <- var15C/var15H
ratio15

lnvar15C <- sd(bucksug15$lnmass[bucksug15$treatment=="C"])^2
lnvar15H <- sd(bucksug15$lnmass[bucksug15$treatment=="H"])^2
lnratio15 <- lnvar15C/lnvar15H
lnratio15

var16C <- sd(bucksug16$mass[bucksug16$treatment=="C"])^2
var16H <- sd(bucksug16$mass[bucksug16$treatment=="H"])^2
ratio16 <- var16C/var16H
ratio16

lnvar16C <- sd(bucksug16$lnmass[bucksug16$treatment=="C"])^2
lnvar16H <- sd(bucksug16$lnmass[bucksug16$treatment=="H"])^2
lnratio16 <- lnvar16C/lnvar16H
lnratio16

varbothC <- sd(bucksugboth$mass[bucksugboth$treatment=="C"])^2
varbothH <- sd(bucksugboth$mass[bucksugboth$treatment=="H"])^2
ratioboth <- varbothC/varbothH
ratioboth

lnvarbothC <- sd(bucksugboth$lnmass[bucksugboth$treatment=="C"])^2
lnvarbothH <- sd(bucksugboth$lnmass[bucksugboth$treatment=="H"])^2
lnratioboth <- lnvarbothC/lnvarbothH
lnratioboth

# Q-Q plots
qqnorm(bucksug15$BRIX)
qqnorm(bucksug16$BRIX)
qqnorm(bucksugboth$BRIX)

qqnorm(bucksug15$mass)
qqnorm(bucksug16$mass)
qqnorm(bucksugboth$mass)

qqnorm(bucksug15$lnmass)
qqnorm(bucksug16$lnmass)
qqnorm(bucksugboth$lnmass)

#Checking spread of key variables
with(bucksugboth, plot(quad, BRIX,  main = "BRIX by quadrant", xlab = "quadrant", ylab = "BRIX"))
with(bucksugboth, plot(quad, mass,  main = "Sugar mass by quadrant", xlab = "quadrant", ylab = "mass"))
with(bucksugboth, plot(quad, lnmass,  main = "ln(sugar mass) by quadrant", xlab = "quadrant", ylab = "ln(mass)"))

with(bucksugboth, plot(date, BRIX,  main = "BRIX by date", xlab = "date", ylab = "BRIX"))
with(bucksugboth, plot(date, mass,  main = "Sugar mass by date", xlab = "date", ylab = "mass"))
with(bucksugboth, plot(date, lnmass,  main = "ln(sugar mass) by date", xlab = "date", ylab = "ln(mass)"))

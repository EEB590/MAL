library(ggplot2)
library(dplyr)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

balssug15 <- read.csv("nectar analysis/data files/balssugar15.csv", header = T)
balssug16 <- read.csv("nectar analysis/data files/balssugar16.csv", header = T)
balssugboth <- rbind(balssug15,balssug16)

balssug15$lnmass <- log(balssug15$mass)
balssug16$lnmass <- log(balssug16$mass)
balssugboth$lnmass <- log(balssugboth$mass)

#Data summaries

summary(balssug15)
summary(balssug16)
summary(balssugboth)

summarize(group_by(balssug15, treatment), meanBRIX = mean(BRIX), sdBRIX = sd(BRIX))
summarize(group_by(balssug16, treatment), meanBRIX = mean(BRIX), sdBRIX = sd(BRIX))
summarize(group_by(balssugboth, treatment), meanBRIX = mean(BRIX), sdBRIX = sd(BRIX))

summarize(group_by(balssug15, treatment), meanmass = mean(mass), sdmass = sd(mass))
summarize(group_by(balssug16, treatment), meanmass = mean(mass), sdmass = sd(mass))
summarize(group_by(balssugboth, treatment), meanmass = mean(mass), sdmass = sd(mass))

qplot(balssug15$BRIX, binwidth = 1)
qplot(balssug16$BRIX, binwidth = 1)
qplot(balssugboth$BRIX, binwidth = 1)

qplot(balssug15$mass, binwidth = 0.005)
qplot(balssug16$mass, binwidth = .0025)
qplot(balssugboth$mass, binwidth = .0025)

qplot(balssug15$lnmass, binwidth = 0.05)
qplot(balssug16$lnmass, binwidth = .05)
qplot(balssugboth$lnmass, binwidth = .05)

ggplot(balssug15, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Balsamroot BRIX 2015")

ggplot(balssug16, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Balsamroot BRIX 2016")

ggplot(balssugboth, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Balsamroot BRIX 2015 & 2016")

ggplot(balssug15, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass") + ggtitle("Balsamroot sugar mass 2015")

ggplot(balssug16, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass") + ggtitle("Balsamroot sugar mass 2016")

ggplot(balssugboth, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass") + ggtitle("Balsamroot sugar mass 2015 & 2016")

# Homoscedastic?

var15C <- sd(balssug15$BRIX[balssug15$treatment=="C"])^2
var15H <- sd(balssug15$BRIX[balssug15$treatment=="H"])^2
ratio15 <- var15H/var15C
ratio15

var16C <- sd(balssug16$BRIX[balssug16$treatment=="C"])^2
var16H <- sd(balssug16$BRIX[balssug16$treatment=="H"])^2
ratio16 <- var16C/var16H
ratio16

varbothC <- sd(balssugboth$BRIX[balssugboth$treatment=="C"])^2
varbothH <- sd(balssugboth$BRIX[balssugboth$treatment=="H"])^2
ratioboth <- varbothC/varbothH
ratioboth

var15C <- sd(balssug15$mass[balssug15$treatment=="C"])^2
var15H <- sd(balssug15$mass[balssug15$treatment=="H"])^2
ratio15 <- var15H/var15C
ratio15

lnvar15C <- sd(balssug15$lnmass[balssug15$treatment=="C"])^2
lnvar15H <- sd(balssug15$lnmass[balssug15$treatment=="H"])^2
lnratio15 <- lnvar15H/lnvar15C
lnratio15

var16C <- sd(balssug16$mass[balssug16$treatment=="C"])^2
var16H <- sd(balssug16$mass[balssug16$treatment=="H"])^2
ratio16 <- var16C/var16H
ratio16

lnvar16C <- sd(balssug16$lnmass[balssug16$treatment=="C"])^2
lnvar16H <- sd(balssug16$lnmass[balssug16$treatment=="H"])^2
lnratio16 <- lnvar16C/lnvar16H
lnratio16

varbothC <- sd(balssugboth$mass[balssugboth$treatment=="C"])^2
varbothH <- sd(balssugboth$mass[balssugboth$treatment=="H"])^2
ratioboth <- varbothH/varbothC
ratioboth

lnvarbothC <- sd(balssugboth$lnmass[balssugboth$treatment=="C"])^2
lnvarbothH <- sd(balssugboth$lnmass[balssugboth$treatment=="H"])^2
lnratioboth <- lnvarbothH/lnvarbothC
lnratioboth

# Q-Q plots
qqnorm(balssug15$BRIX)
qqnorm(balssug16$BRIX)
qqnorm(balssugboth$BRIX)

qqnorm(balssug15$mass)
qqnorm(balssug16$mass)
qqnorm(balssugboth$mass)

qqnorm(balssug15$lnmass)
qqnorm(balssug16$lnmass)
qqnorm(balssugboth$lnmass)

# Plots of response by variable
  # by date
with(balssugboth, plot(date, BRIX, main = "BRIX by date", xlab = "date", ylab = "BRIX"))
with(balssugboth, plot(date, mass, main = "Sugar mass by date", xlab = "date", ylab = "mass"))
with(balssugboth, plot(date, lnmass, main = "ln(sugar mass) by date", xlab = "date", ylab = "mass"))

  # by plot number
with(balssugboth, plot(plot, BRIX, main = "BRIX by plot", xlab = "plot", ylab = "BRIX"))
with(balssugboth, plot(plot, mass, main = "Sugar mass by plot", xlab = "plot", ylab = "mass"))
with(balssugboth, plot(plot, lnmass, main = "ln(sugar mass) by plot", xlab = "plot", ylab = "mass"))

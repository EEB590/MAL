library(ggplot2)
library(dplyr)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

buckvol15 <- read.csv("nectar analysis/data files/buckvol15.csv", header = T)
buckvol16 <- read.csv("nectar analysis/data files/buckvol16.csv", header = T)
buckvolboth <- rbind(buckvol15,buckvol16)

buckvol15$lnvol <- log(buckvol15$volume)
buckvol16$lnvol <- log(buckvol16$volume)
buckvolboth$lnvol <- log(buckvolboth$volume)

#Data summaries

summary(buckvol15)
summary(buckvol16)
summary(buckvolboth)

summarize(group_by(buckvol15, treatment), meanVol = mean(volume), sdVolume = sd(volume))
summarize(group_by(buckvol16, treatment), meanVol = mean(volume), sdVolume = sd(volume))
summarize(group_by(buckvolboth, treatment), meanVol = mean(volume), sdVolume = sd(volume))

qplot(buckvol15$volume, binwidth = .025)
qplot(buckvol16$volume, binwidth = .005)
qplot(buckvolboth$volume, binwidth = .025)

qplot(buckvol15$lnvol, binwidth = .05)
qplot(buckvol16$lnvol, binwidth = .05)
qplot(buckvolboth$lnvol, binwidth = .05)

ggplot(buckvol15, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Buckwheat Volume 2015")

ggplot(buckvol16, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Buckwheat Volume 2016")

ggplot(buckvolboth, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Buckwheat Volume 2015 & 2016")

# Homoscedastic?

var15C <- sd(buckvol15$volume[buckvol15$treatment=="C"])^2
var15H <- sd(buckvol15$volume[buckvol15$treatment=="H"])^2
ratio15 <- var15C/var15H
ratio15

lnvar15C <- sd(buckvol15$lnvol[buckvol15$treatment=="C"])^2
lnvar15H <- sd(buckvol15$lnvol[buckvol15$treatment=="H"])^2
lnratio15 <- lnvar15C/lnvar15H
lnratio15

var16C <- sd(buckvol16$volume[buckvol16$treatment=="C"])^2
var16H <- sd(buckvol16$volume[buckvol16$treatment=="H"])^2
ratio16 <- var16C/var16H
ratio16

lnvar16C <- sd(buckvol16$lnvol[buckvol16$treatment=="C"])^2
lnvar16H <- sd(buckvol16$lnvol[buckvol16$treatment=="H"])^2
lnratio16 <- lnvar16C/lnvar16H
lnratio16

varbothC <- sd(buckvolboth$volume[buckvolboth$treatment=="C"])^2
varbothH <- sd(buckvolboth$volume[buckvolboth$treatment=="H"])^2
ratioboth <- varbothC/varbothH
ratioboth

lnvarbothC <- sd(buckvolboth$lnvol[buckvolboth$treatment=="C"])^2
lnvarbothH <- sd(buckvolboth$lnvol[buckvolboth$treatment=="H"])^2
lnratioboth <- lnvarbothC/lnvarbothH
lnratioboth

# Q-Q plots

qqnorm(buckvol15$volume)
qqnorm(buckvol16$volume)
qqnorm(buckvolboth$volume)

qqnorm(buckvol15$lnvol)
qqnorm(buckvol16$lnvol)
qqnorm(buckvolboth$lnvol)

#Checking spread of key variables
with(buckvolboth, plot(quad, volume, main = "Volume by quadrant", xlab = "quadrant", ylab = "volume"))
with(buckvolboth, plot(date, volume, main = "Volume by date", xlab = "date", ylab = "volume"))
with(buckvolboth, plot(date, lnvol, main = "ln(volume) by date", xlab = "date", ylab = "ln(volume)"))

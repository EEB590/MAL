library(ggplot2)
library(dplyr)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

balsvol15 <- read.csv("nectar analysis/data files/balsvol15.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)
balsvolboth <- rbind(balsvol15,balsvol16)

balsvol15$lnvol <- log(balsvol15$volume)
balsvol16$lnvol <- log(balsvol16$volume)
balsvolboth$lnvol <- log(balsvolboth$volume)

#Data summaries

summary(balsvol15)
summary(balsvol16)
summary(balsvolboth)

summarize(group_by(balsvol15, treatment), meanVol = mean(volume), sdVolume = sd(volume))
summarize(group_by(balsvol16, treatment), meanVol = mean(volume), sdVolume = sd(volume))
summarize(group_by(balsvolboth, treatment), meanVol = mean(volume), sdVolume = sd(volume))

qplot(balsvol15$volume, binwidth = .025)
qplot(balsvol16$volume, binwidth = .01)
qplot(balsvolboth$volume, binwidth = .025)

qplot(balsvol15$lnvol, binwidth = .05)
qplot(balsvol16$lnvol, binwidth = .05)
qplot(balsvolboth$lnvol, binwidth = .05)

ggplot(balsvol15, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2015")

ggplot(balsvol16, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2016")

ggplot(balsvolboth, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2015 & 2016")

# Homoscedastic?

var15C <- sd(balsvol15$volume[balsvol15$treatment=="C"])^2
var15H <- sd(balsvol15$volume[balsvol15$treatment=="H"])^2
ratio15 <- var15H/var15C
ratio15

lnvar15C <- sd(balsvol15$lnvol[balsvol15$treatment=="C"])^2
lnvar15H <- sd(balsvol15$lnvol[balsvol15$treatment=="H"])^2
lnratio15 <- lnvar15H/lnvar15C
lnratio15

var16C <- sd(balsvol16$volume[balsvol16$treatment=="C"])^2
var16H <- sd(balsvol16$volume[balsvol16$treatment=="H"])^2
ratio16 <- var16C/var16H
ratio16

lnvar16C <- sd(balsvol16$lnvol[balsvol16$treatment=="C"])^2
lnvar16H <- sd(balsvol16$lnvol[balsvol16$treatment=="H"])^2
lnratio16 <- lnvar16H/lnvar16C
lnratio16

varbothC <- sd(balsvolboth$volume[balsvolboth$treatment=="C"])^2
varbothH <- sd(balsvolboth$volume[balsvolboth$treatment=="H"])^2
ratioboth <- varbothH/varbothC
ratioboth

lnvarbothC <- sd(balsvolboth$lnvol[balsvolboth$treatment=="C"])^2
lnvarbothH <- sd(balsvolboth$lnvol[balsvolboth$treatment=="H"])^2
lnratioboth <- lnvarbothH/lnvarbothC
lnratioboth

# Q-Q plots

qqnorm(balsvol15$volume)
qqnorm(balsvol16$volume)
qqnorm(balsvolboth$volume)

qqnorm(balsvol15$lnvol)
qqnorm(balsvol16$lnvol)
qqnorm(balsvolboth$lnvol)

# Plots of response by variable
  # by date
with(balsvolboth, plot(date, volume, main = "Volume by date", xlab = "Date", ylab = "Volume"))
with(balsvolboth, plot(plot, volume, main = "Volume by plot", xlab = "Plot", ylab = "volume"))

with(balsvolboth, plot(date, lnvol, main = "ln(volume) by date", xlab = "Date", ylab = "Volume"))
with(balsvolboth, plot(plot, lnvol, main = "ln(volume) by plot", xlab = "Plot", ylab = "volume"))


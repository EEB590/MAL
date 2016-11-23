library(ggplot2)
library(dplyr)
library(mvnormtest)
library(HH)

balsvol15 <- read.csv("nectar analysis/data files/balsvol15.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)
balsvolboth <- rbind(balsvol15,balsvol16)

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

ggplot(balsvol15, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2015")

ggplot(balsvol16, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2016")

ggplot(balsvolboth, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2015 & 2016")

# Homoscedastic?

var15C <- sd(balsvol15$volume[balsvol15$treatment=="C"])^2
var15H <- sd(balsvol15$volume[balsvol15$treatment=="H"])^2
ratio15 <- var15H/var15C
ratio15

var16C <- sd(balsvol16$volume[balsvol16$treatment=="C"])^2
var16H <- sd(balsvol16$volume[balsvol16$treatment=="H"])^2
ratio16 <- var16C/var16H
ratio16

varbothC <- sd(balsvolboth$volume[balsvolboth$treatment=="C"])^2
varbothH <- sd(balsvolboth$volume[balsvolboth$treatment=="H"])^2
ratioboth <- varbothH/varbothC
ratioboth

# Q-Q plots

qqnorm(balsvol15$volume)
qqnorm(balsvol16$volume)
qqnorm(balsvolboth$volume)


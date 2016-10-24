library(ggplot2)
library(GGally)
library(dplyr)
library(mvnormtest)
library(HH)
library(Rcmdr)
library(nlme)

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
sugar.buck$trans.conc <- (sugar.buck$BRIX^(2))
head(sugar.buck)

  # Subset for 2015

volume.buck2015 <- as.data.frame(volume.buck[volume.buck$Year.Factor == "1",])
sugar.buck2015 <- as.data.frame(sugar.buck[sugar.buck$Year.Factor == "1",])

#Data summaries

summary(volume.buck2015)
summarize(group_by(volume.buck2015, Heat), meanVol = mean(Volume), sdVolume = sd(Volume))
summary(sugar.buck2015)
summarize(group_by(sugar.buck2015, Heat), meanBRIX = mean(BRIX), meanMass = mean(Mass), sdBRIX = sd(BRIX), sdMass = sd(Mass))

qplot(volume.buck2015$Volume, binwidth = 0.01)
qplot(volume.buck2015$trans.vol, binwidth = 0.01)
qplot(sugar.buck2015$BRIX, binwidth = 1)
qplot(sugar.buck2015$trans.conc, binwidth = 50)
qplot(sugar.buck2015$Mass, binwidth = 5)

#Volume
ggplot(volume.buck2015, aes(x=Heat, y=trans.vol)) + geom_boxplot() +
  xlab("Treatment") + scale_x_discrete(labels=c("Control", "Heat")) +
  ylab("Nectar Volume (microliters)") + ggtitle("Buckwheat Volume (log transformed)")

ggplot(volume.buck2015, aes(x=Heat, y=Volume)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Nectar Volume (microliters)") + ggtitle("Buckwheat Volume (untransformed)")


ggplot(sugar.buck2015, aes(x=Heat, y=trans.conc)) + geom_boxplot() +
  xlab("Treatment") + scale_x_discrete(labels=c("Control", "Heat")) +
  ylab("Sugar concentration (%m/m)") + ggtitle("Buckwheat Sugar Concentration (transformed data)")

ggplot(sugar.buck2015, aes(x=Heat, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") + scale_x_discrete(labels=c("Control", "Heat")) +
  ylab("Sugar concentration (%g/g)") + ggtitle("Buckwheat Sugar Concentration")


ggplot(sugar.buck2015, aes(x=Heat, y=Mass)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Sugar mass (mg)") + ggtitle("Buckwheat Sugar Mass")


# Test normality for volume by treatment
shapiro.test(as.matrix(volume.buck2015[volume.buck2015[,6] == "0", 8])) #control
shapiro.test(as.matrix(volume.buck2015[volume.buck2015[,6] == "1", 8])) #heat treatment

# Test normality for sugar concentration by treatment
shapiro.test(as.matrix(sugar.buck2015[sugar.buck2015[,5] == "0", 8])) #control
shapiro.test(as.matrix(sugar.buck2015[sugar.buck2015[,5] == "1", 8])) #heat treatment

# Test normality for sugar mass by treatment
shapiro.test(as.matrix(sugar.buck2015[sugar.buck2015[,5] == "0", 7])) #control
shapiro.test(as.matrix(sugar.buck2015[sugar.buck2015[,5] == "1", 7])) #heat treatment

#Levene test for Homogineity of variances
leveneTest(volume.buck2015[,8],volume.buck2015[,5]) #Volume
leveneTest(sugar.buck2015[,8],sugar.buck2015[,5]) #Concentration
leveneTest(sugar.buck2015[,7],sugar.buck2015[,5]) #Mass

#Fligner test for homogineity of variances
fligner.test(volume.buck2015[,8],volume.buck2015[,5]) #Volume
fligner.test(sugar.buck2015[,8],sugar.buck2015[,5]) #Concentration
fligner.test(sugar.buck2015[,7],sugar.buck2015[,5]) #Mass

qqnorm(volume.buck2015$trans.vol)
qqnorm(sugar.buck2015$trans.conc)
qqnorm(sugar.buck2015$Mass)

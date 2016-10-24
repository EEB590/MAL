library(ggplot2)
library(GGally)
library(dplyr)
library(mvnormtest)
library(HH)
library(Rcmdr)
library(nlme)

######################
### 2015-2016 data ###
######################

# Create the  data frames

##Read in the data

setwd("D:/Iowa State University/Debinski Lab/Nectar data/Nectar analysis for manuscript")

  # Balsamroot volume, 2015-2016

volume.balsam <- read.csv("Nectar_Vol_Balsam.csv", header = T, col.names = c("Date", "Date.Factor", "Year.Factor", "Plot", "Heat", "Plant", "Volume"))
volume.balsam <- data.frame(volume.balsam[,1:7])
volume.balsam$Date.Factor <- as.factor(volume.balsam$Date.Factor)
volume.balsam$Year.Factor <- as.factor(volume.balsam$Year.Factor) # 1 = 2015, 2 = 2016
volume.balsam$Heat <- as.factor(volume.balsam$Heat) # 0 = control, 1 = heat
volume.balsam$Plant <- as.factor(paste(volume.balsam$Plot,volume.balsam$Plant,sep="-"))
volume.balsam$Plot <- as.factor(volume.balsam$Plot)
volume.balsam$log.vol <- log(volume.balsam$Volume)
head(volume.balsam)

  # Balsamroot sugar, 2015-2016

sugar.balsam <- read.csv("Nectar_BRIX_Balsam.csv", header = T, col.names = c("Date", "Date.Factor", "Year.Factor", "Plot", "Heat","Plant", "BRIX", "Mass"))
sugar.balsam <- data.frame(sugar.balsam[,1:8])
sugar.balsam$Date.Factor <- as.factor(sugar.balsam$Date.Factor)
sugar.balsam$Year.Factor <- as.factor(sugar.balsam$Year.Factor)
sugar.balsam$Heat <- as.factor(sugar.balsam$Heat)
sugar.balsam$Plant <- as.factor(paste(sugar.balsam$Plot,sugar.balsam$Plant,sep="-"))
sugar.balsam$Plot <- as.factor(sugar.balsam$Plot)
sugar.balsam$log.mass <- log(sugar.balsam$Mass)
head(sugar.balsam)

#Data summaries

summary(volume.balsam)
summarize(group_by(volume.balsam, Heat), meanVol = mean(Volume), sdVolume = sd(Volume))
summary(sugar.balsam)
summarize(group_by(sugar.balsam, Heat), meanBRIX = mean(BRIX), meanMass = mean(Mass), sdBRIX = sd(BRIX), sdMass = sd(Mass))

qplot(volume.balsam$Volume, binwidth = 0.05)
qplot(volume.balsam$log.vol, binwidth = .05)
qplot(volume.balsam$log.vol[volume.balsam$Year.Factor == "1"], binwidth = .05)
qplot(volume.balsam$log.vol[volume.balsam$Year.Factor == "2"], binwidth = .05)

qplot(sugar.balsam$BRIX, binwidth = 1)
qplot(sugar.balsam$BRIX[sugar.balsam$Year.Factor == "1"], binwidth = 1)
qplot(sugar.balsam$BRIX[sugar.balsam$Year.Factor == "2"], binwidth = 1)

qplot(sugar.balsam$Mass, binwidth = 0.01)
qplot(sugar.balsam$log.mass, binwidth = .05)
qplot(sugar.balsam$log.mass[sugar.balsam$Year.Factor == "1"], binwidth = .05)
qplot(sugar.balsam$log.mass[sugar.balsam$Year.Factor == "2"], binwidth = .05)

ggplot(volume.balsam, aes(x=Heat, y=log.vol)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume (log transformed), 2015-2016")

ggplot(sugar.balsam, aes(x=Heat, y=BRIX)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Sugar concentration (%m/m)") + ggtitle("Balsamroot Sugar Concentration, 2015-2016")

ggplot(sugar.balsam, aes(x=Heat, y=log.mass)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Sugar mass (mg)") + ggtitle("Balsamroot Sugar Mass (log transformed), 2015-2016")


#Normality tests
  # Test normality for volume by treatment, 2015-2016
shapiro.test(as.matrix(volume.balsam[volume.balsam[,5] == "0", 8])) #control
shapiro.test(as.matrix(volume.balsam[volume.balsam[,5] == "1", 8])) #heat treatment

  # Test normality for sugar concentration by treatment, 2015-2016
shapiro.test(as.matrix(sugar.balsam[sugar.balsam[,5] == "0", 7])) #control
shapiro.test(as.matrix(sugar.balsam[sugar.balsam[,5] == "1", 7])) #heat treatment

  # Test normality for sugar mass by treatment, 2015-2016
shapiro.test(as.matrix(sugar.balsam[sugar.balsam[,5] == "0", 9])) #control
shapiro.test(as.matrix(sugar.balsam[sugar.balsam[,5] == "1", 9])) #heat treatment


#Homoscedasticity tests

#Levene test for Homogineity of variances
leveneTest(volume.balsam[,8],volume.balsam[,5]) #Volume
leveneTest(sugar.balsam[,7],sugar.balsam[,5]) #Concentration
leveneTest(sugar.balsam[,9],sugar.balsam[,5]) #Mass

#Fligner test for homogineity of variances
fligner.test(volume.balsam[,8],volume.balsam[,5]) #Volume
fligner.test(sugar.balsam[,7],sugar.balsam[,5]) #Concentration
fligner.test(sugar.balsam[,9],sugar.balsam[,5]) #Mass

qqnorm(volume.balsam$log.vol)
qqnorm(sugar.balsam$BRIX)
qqnorm(sugar.balsam$log.mass)


#######################
###  2016 only data ###
#######################

#Data summaries

volume.balsam2016 <- as.data.frame(volume.balsam[volume.balsam$Year.Factor == "2",])
summary(volume.balsam2016)
summarize(group_by(volume.balsam2016, Heat), meanVol = mean(Volume), sdVolume = sd(Volume))
sugar.balsam2016 <- as.data.frame((sugar.balsam[sugar.balsam$Year.Factor == "2",]))
summary(sugar.balsam2016)
summarize(group_by(sugar.balsam2016, Heat), meanBRIX = mean(BRIX), meanMass = mean(Mass), sdBRIX = sd(BRIX), sdMass = sd(Mass))

qplot(volume.balsam2016$log.vol, binwidth = .05)
qplot(sugar.balsam2016$BRIX, binwidth = 1)
qplot(sugar.balsam2016$log.mass, binwidth = .05)

ggplot(volume.balsam2016, aes(x=Heat, y=log.vol)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume (log transformed), 2016 only")

ggplot(sugar.balsam2016, aes(x=Heat, y=BRIX)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Sugar concentration (%m/m)") + ggtitle("Balsamroot Sugar Concentration, 2016 only")

ggplot(sugar.balsam2016, aes(x=Heat, y=log.mass)) + geom_boxplot() +
  xlab("Treatment: 0 = Control, 1 = Heat") +
  ylab("Sugar mass (mg)") + ggtitle("Balsamroot Sugar Mass (log transformed), 2016 only")


#Normality tests

# Test normality for volume by treatment, 2016 only
shapiro.test(as.matrix(volume.balsam2016[volume.balsam2016[,5] == "0", 8])) #control
shapiro.test(as.matrix(volume.balsam2016[volume.balsam2016[,5] == "1", 8])) #heat treatment

# Test normality for sugar concentration by treatment, 2016 only
shapiro.test(as.matrix(sugar.balsam2016[sugar.balsam2016[,5] == "0", 7])) #control
shapiro.test(as.matrix(sugar.balsam2016[sugar.balsam2016[,5] == "1", 7])) #heat treatment

# Test normality for sugar mass by treatment, 2016
shapiro.test(as.matrix(sugar.balsam2016[sugar.balsam2016[,5] == "0", 9])) #control
shapiro.test(as.matrix(sugar.balsam2016[sugar.balsam2016[,5] == "1", 9])) #heat treatment


#Homoscedasticity tests

#Levene test for Homogineity of variances
leveneTest(volume.balsam2016[,8],volume.balsam2016[,5]) #Volume
leveneTest(sugar.balsam2016[,7],sugar.balsam2016[,5]) #Concentration
leveneTest(sugar.balsam2016[,9],sugar.balsam2016[,5]) #Mass

#Fligner test for homogineity of variances
fligner.test(volume.balsam2016[,8],volume.balsam2016[,5]) #Volume
fligner.test(sugar.balsam2016[,7],sugar.balsam2016[,5]) #Concentration
fligner.test(sugar.balsam2016[,9],sugar.balsam2016[,5]) #Mass

qqnorm(volume.balsam2016$log.vol)
qqnorm(sugar.balsam2016$BRIX)
qqnorm(sugar.balsam2016$log.mass)


#######################
###  2015 only data ###
#######################

#Data summaries

volume.balsam2015 <- as.data.frame(volume.balsam[volume.balsam$Year.Factor == "1",])
summary(volume.balsam2015)
summarize(group_by(volume.balsam2015, Heat), meanVol = mean(Volume), sdVolume = sd(Volume))
sugar.balsam2015 <- as.data.frame((sugar.balsam[sugar.balsam$Year.Factor == "1",]))
summary(sugar.balsam2015)
summarize(group_by(sugar.balsam2015, Heat), meanBRIX = mean(BRIX), meanMass = mean(Mass), sdBRIX = sd(BRIX), sdMass = sd(Mass))

qplot(volume.balsam2015$Volume, binwidth = .025)
qplot(volume.balsam2015$log.vol, binwidth = .05)
qplot(sugar.balsam2015$BRIX, binwidth = 1)
qplot(sugar.balsam2015$Mass, binwidth = .005)
qplot(sugar.balsam2015$log.mass, binwidth = .05)

ggplot(volume.balsam2015, aes(x=Heat, y=log.vol)) + geom_boxplot() +
  xlab("Treatment") + scale_x_discrete(labels=c("Control", "Heat")) +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume (log transformed)")

ggplot(sugar.balsam2015, aes(x=Heat, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +  scale_x_discrete(labels=c("Control", "Heat")) +
  ylab("Sugar concentration (%m/m)") + ggtitle("Balsamroot Sugar Concentration")

ggplot(sugar.balsam2015, aes(x=Heat, y=log.mass)) + geom_boxplot() +
  xlab("Treatment") +  scale_x_discrete(labels=c("Control", "Heat")) +
  ylab("Sugar mass (mg)") + ggtitle("Balsamroot Sugar Mass (log transformed)")


#Normality tests

# Test normality for volume by treatment, 2016 only
shapiro.test(as.matrix(volume.balsam2015[volume.balsam2015[,5] == "0", 8])) #control
shapiro.test(as.matrix(volume.balsam2015[volume.balsam2015[,5] == "1", 8])) #heat treatment

# Test normality for sugar concentration by treatment, 2016 only
shapiro.test(as.matrix(sugar.balsam2015[sugar.balsam2015[,5] == "0", 7])) #control
shapiro.test(as.matrix(sugar.balsam2015[sugar.balsam2015[,5] == "1", 7])) #heat treatment

# Test normality for sugar mass by treatment, 2016
shapiro.test(as.matrix(sugar.balsam2015[sugar.balsam2015[,5] == "0", 9])) #control
shapiro.test(as.matrix(sugar.balsam2015[sugar.balsam2015[,5] == "1", 9])) #heat treatment


#Homoscedasticity tests

#Levene test for Homogineity of variances
leveneTest(volume.balsam2015[,8],volume.balsam2015[,5]) #Volume
leveneTest(sugar.balsam2015[,7],sugar.balsam2015[,5]) #Concentration
leveneTest(sugar.balsam2015[,9],sugar.balsam2015[,5]) #Mass

#Fligner test for homogineity of variances
fligner.test(volume.balsam2015[,8],volume.balsam2015[,5]) #Volume
fligner.test(sugar.balsam2015[,7],sugar.balsam2015[,5]) #Concentration
fligner.test(sugar.balsam2015[,9],sugar.balsam2015[,5]) #Mass

qqnorm(volume.balsam2015$log.vol)
qqnorm(sugar.balsam2015$BRIX)
qqnorm(sugar.balsam2015$log.mass)

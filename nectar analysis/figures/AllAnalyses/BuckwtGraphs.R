library(ggplot2)
library(dplyr)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

buckvol15 <- read.csv("nectar analysis/data files/buckvol15.csv", header = T)
buckvol16 <- read.csv("nectar analysis/data files/buckvol16.csv", header = T)
buckvolboth <- rbind(buckvol15,buckvol16)

bucksug15 <- read.csv("nectar analysis/data files/bucksugar15.csv", header = T)
bucksug16 <- read.csv("nectar analysis/data files/bucksugar16.csv", header = T)
bucksugboth <- rbind(bucksug15,bucksug16)

buckvol15$lnvol <- log(buckvol15$volume)
buckvol16$lnvol <- log(buckvol16$volume)
buckvolboth$lnvol <- log(buckvolboth$volume)

bucksug15$lnmass <- log(bucksug15$mass)
bucksug16$lnmass <- log(bucksug16$mass)
bucksugboth$lnmass <- log(bucksugboth$mass)

#2015

ggplot(buckvol15, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Buckwheat Volume 2015")

ggplot(buckvol15, aes(x=treatment, y=lnvol)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters) (log transformed)") + ggtitle("Buckwheat Volume 2015 (log transformed)")

ggplot(bucksug15, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Buckwheat BRIX 2015")

ggplot(bucksug15, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg)") + ggtitle("Buckwheat sugar mass 2015")

ggplot(bucksug15, aes(x=treatment, y=lnmass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg) (log transformed)") + ggtitle("Buckwheat sugar mass 2015 (log transformed)")


#2016

ggplot(buckvol16, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Buckwheat Volume 2016")

ggplot(buckvol16, aes(x=treatment, y=lnvol)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters) (log transformed)") + ggtitle("Buckwheat Volume 2016 (log transformed)")

ggplot(bucksug16, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Buckwheat BRIX 2016")

ggplot(bucksug16, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg)") + ggtitle("Buckwheat sugar mass 2016")

ggplot(bucksug16, aes(x=treatment, y=lnmass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg) (log transformed)") + ggtitle("Buckwheat sugar mass 2016 (log transformed)")


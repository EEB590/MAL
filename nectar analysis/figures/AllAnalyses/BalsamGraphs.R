library(ggplot2)
library(dplyr)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

balsvol15 <- read.csv("nectar analysis/data files/balsvol15.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)
balsvolboth <- rbind(balsvol15,balsvol16)

balssug15 <- read.csv("nectar analysis/data files/balssugar15.csv", header = T)
balssug16 <- read.csv("nectar analysis/data files/balssugar16.csv", header = T)
balssugboth <- rbind(balssug15,balssug16)

balsvol15$lnvol <- log(balsvol15$volume)
balsvol16$lnvol <- log(balsvol16$volume)
balsvolboth$lnvol <- log(balsvolboth$volume)

balssug15$lnmass <- log(balssug15$mass)
balssug16$lnmass <- log(balssug16$mass)
balssugboth$lnmass <- log(balssugboth$mass)

#2015

ggplot(balsvol15, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2015")

ggplot(balsvol15, aes(x=treatment, y=lnvol)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters) (log transformed)") + ggtitle("Balsamroot Volume 2015 (log transformed)")

ggplot(balssug15, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Balsamroot BRIX 2015")

ggplot(balssug15, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg)") + ggtitle("Balsamroot sugar mass 2015")

ggplot(balssug15, aes(x=treatment, y=lnmass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg) (log transformed)") + ggtitle("Balsamroot sugar mass 2015 (log transformed)")


#2016

ggplot(balsvol16, aes(x=treatment, y=volume)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters)") + ggtitle("Balsamroot Volume 2016")

ggplot(balsvol16, aes(x=treatment, y=lnvol)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar Volume (microliters) (log transformed)") + ggtitle("Balsamroot Volume 2016 (log transformed)")

ggplot(balssug16, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Balsamroot BRIX 2016")

ggplot(balssug16, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg)") + ggtitle("Balsamroot sugar mass 2016")

ggplot(balssug16, aes(x=treatment, y=lnmass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg) (log transformed)") + ggtitle("Balsamroot sugar mass 2016 (log transformed)")


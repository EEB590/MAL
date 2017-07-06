library(lme4)
library(lsmeans)
library(ggplot2)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

flowers <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/TotalFlowersPerPlant.csv", header = T, as.is = T)

flowers$total <- apply(flowers[4:5], 1, sum)
names(flowers)[4:5] <- c("year15", "year16")
flowers$plot <- as.factor(flowers$plot)
flowers$treatment <- as.factor(flowers$treatment)
flowers$plantid <- as.factor(flowers$plantid)

qplot(flowers$year15, binwidth = 1, xlab = "# of flowers", main = "2015")
qplot(flowers$year16, binwidth = 1, xlab = "# of flowers", main = "2016")
qplot(flowers$total, binwidth = 1, xlab = "# of flowers", main = "2015 and 2016 totaled")

ggplot(flowers, aes(x = year15)) + geom_histogram(binwidth = 1) + facet_grid(treatment~.)
ggplot(flowers, aes(x = year16)) + geom_histogram(binwidth = 1) + facet_grid(treatment~.)
ggplot(flowers, aes(x = total)) + geom_histogram(binwidth = 1) + facet_grid(treatment~.)

qplot(flowers$treatment, flowers$year15, geom = "boxplot", xlab = "Treatment", ylab = "# of flowers per plant", main = "2015")
qplot(flowers$treatment, flowers$year16, geom = "boxplot", xlab = "Treatment", ylab = "# of flowers per plant", main = "2016")
qplot(flowers$treatment, flowers$total, geom = "boxplot", xlab = "Treatment", ylab = "# of flowers per plant", main = "2015 and 2016 totaled")

#model?

#model?
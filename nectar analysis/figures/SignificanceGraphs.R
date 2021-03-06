library(ggplot2)
library(dplyr)
library(reshape)
library(gridExtra)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

#Balsamroot
balssug15 <- read.csv("nectar analysis/data files/balssugar15.csv", header = T)
balssug16 <- read.csv("nectar analysis/data files/balssugar16.csv", header = T)
balssug15$lnmass <- log(balssug15$mass)
balssug16$lnmass <- log(balssug16$mass)

balspa <- read.csv("nectar analysis/data files/balsam15.csv", header = T)
balspa$necpres[balspa$volume != "0"] <- "1"
balspa$necpres[balspa$volume == "0"] <- "0"
balspa$necpres <- as.factor(balspa$necpres)
balspa <- balspa[,-c(2,4:7)]
balspa$datechr <- as.character(balspa$date)
balspa$datechr <- gsub("2015-", "", balspa$datechr)
balspa$datechr <- gsub("06-", "June ", balspa$datechr)
balspa$datechr <- as.factor(balspa$datechr)
balspa <- balspa[,2:4]

#Buckwheat
buckvol15 <- read.csv("nectar analysis/data files/buckvol15.csv", header = T)
bucksug15 <- read.csv("nectar analysis/data files/bucksugar15.csv", header = T)
buckvol15$lnvol <- log(buckvol15$volume)


#Plots

bals15brix <- ggplot(balssug15, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Balsamroot BRIX 2015")

bals16mass <- ggplot(balssug16, aes(x=treatment, y=mass)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar sugar mass (mg)") + ggtitle("Balsamroot Sugar Mass 2016")

buck15volln <- ggplot(buckvol15, aes(x=treatment, y=lnvol)) + geom_boxplot() +
  labs( x = "Treatment", y = "Nectar Volume (microliters)", title = "Buckwheat Volume 2015", subtitle = "(log transformed)")

buck15brix <- ggplot(bucksug15, aes(x=treatment, y=BRIX)) + geom_boxplot() +
  xlab("Treatment") +
  ylab("Nectar BRIX") + ggtitle("Buckwheat BRIX 2015")

prestable <- table(balspa)
presdf <- melt(prestable); rm(prestable)
totalsdf <- summarize(group_by(presdf, datechr, treatment), sum(value))
presdf <- merge(presdf, totalsdf); rm(totalsdf)
presdf <- presdf[which(presdf$necpres == 1),]
presdf$percent <- presdf$value/presdf$`sum(value)` * 100
levels(presdf$datechr) <- c(levels(presdf$datechr), "June 04", "June 05", "June 07", "June 09")

necpres <- ggplot(presdf, aes(x = datechr, y = percent, color = treatment, group = treatment)) +
  geom_line() + theme(axis.text.x = element_text(angle=20, hjust=1, vjust=1)) +
  theme(axis.title.x=element_blank()) +
  labs(y = "Percent", title = "Percent of Flowers that Contain Nectar",
       subtitle = "Balsamroot 2015", color = "Treatment")

flowers <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/TotalFlowersPerPlant.csv", header = T, as.is = T)
names(flowers)[4:5] <- c("count15", "count16")
flowers$treatment <- as.factor(flowers$treatment)

g1 <- ggplot(flowers, aes(x = treatment, y = count15)) + geom_boxplot() +
  ylim(0, 50) + labs(title = "2015", x = NULL, y = "Count")
g2 <- ggplot(flowers, aes(x = treatment, y = count16)) + geom_boxplot() +
  ylim(0, 50) + labs(title = "2016", x = NULL, y = NULL)
totflow <- grid.arrange(g1, g2, ncol = 2, top = grid::textGrob("Total # of Flowers, Balsamroot",x=0.05,hjust=0), bottom = "Treatment")

grid.arrange(bals15brix, bals16mass, buck15brix, buck15volln, necpres, totflow, ncol=2)


###  Old code
#flowers <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/TotalFlowersPerPlant.csv", header = T, as.is = T)
#flowers$total <- apply(flowers[4:5], 1, sum)
#flowers <- flowers[,-c(1,3:5)]
#flowers$treatment <- as.factor(flowers$treatment)

#totflow <- ggplot(flowers, aes(x = total)) + geom_histogram(binwidth = 1) + facet_grid(treatment~.) +
#  labs(title = "Total Flowers", subtitle = "by treatment, Balsamroot 2015 + 2016", x = "Total Flowers", y = "Count of Plants") 

#necpa <- ggplot(balspa, aes(x = datechr, fill = necpres)) +
#  theme(axis.text.x = element_text(angle=20, hjust=1, vjust=1)) +
#  theme(axis.title.x=element_blank()) +
#  geom_bar(position = "fill") +
#  scale_fill_manual(values=c("gray30", "grey70"), labels=c("Absent", "Present")) +
#  labs(y = "Percent", title = "Nectar Presence/Absence", subtitle = "by treatment, Balsamroot 2015") +
#  guides(fill=guide_legend(title=NULL, reverse = TRUE)) +
#  facet_grid(treatment~.)




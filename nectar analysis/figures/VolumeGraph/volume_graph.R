library(ggplot2)
library(tidyr)

Opler <- read.csv("nectar analysis/figures/VolumeGraph/Volumes.csv", header = T)

opler <- gather(Opler, "group", "volume", 1:6)
rm(Opler)
opler <- na.omit(opler)
opler$group <- as.factor(opler$group)

oplersm <- opler[which(opler$group == "Butterfly" | opler$group == "Lg_Bee" | opler$group == "Sm_Bee_Wasp"),]

buck15 <- read.csv("nectar analysis/data files/buckwt15.csv")

meanC <- mean(subset(buck15, treatment == "C")$volume)
meanH <- mean(subset(buck15, treatment == "H")$volume)
sdC <- sd(subset(buck15, treatment == "C")$volume)
sdH <- sd(subset(buck15, treatment == "H")$volume)

ggplot(oplersm, aes(x=factor(group), y = volume)) +
  geom_boxplot() +
  scale_y_log10() +
  xlab("Functional Group") +
  ylab("Volume of Nectar (microliters, log scale)") +
  ggtitle("Nectar Volume Preferences") +
  geom_hline(yintercept = meanH, col = "red") +
  geom_hline(yintercept = meanC, col = "green")

ggplot(oplersm, aes(x=factor(group), y = volume)) +
  geom_boxplot() +
  scale_y_log10() +
  xlab("Functional Group") +
  ylab("Volume of Nectar (microliters, log scale)") +
  ggtitle("Nectar Volume Preferences") +
  geom_hline(yintercept = meanH, col = "red") +
  geom_hline(yintercept = meanC, col = "green") +
  geom_hline(yintercept = meanC+sdC, col = "green", linetype = "dashed") +
  geom_hline(yintercept = meanC-sdC, col = "green", linetype = "dashed") +
  geom_hline(yintercept = meanH+sdH, col = "red", linetype = "dashed") +
  geom_hline(yintercept = meanH-sdH, col = "red", linetype = "dashed")


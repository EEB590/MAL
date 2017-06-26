library(ggplot2)
library(lme4)
library(nlme)
library(lsmeans)
library(lubridate)
library(multcompView)
library(car)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

balsvol15 <- read.csv("nectar analysis/data files/balsvol15.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)
balsvolboth <- rbind(balsvol15,balsvol16)
rm(balsvol15)
rm(balsvol16)

balsvolboth$lnvol <- log(balsvolboth$volume)
balsvolboth$year <- as.factor(year(balsvolboth$date))

cellN <- with(balsvolboth, table(treatment, year))
cellN

cellMean <- with(balsvolboth, tapply(volume, list(treatment, year), mean))
cellMean

# Model ln(volume), hypothesis test
  #modeled ln of volume because using volume itself doesn't give us good residuals
modlnvol <- lmer(lnvol ~ treatment * year + (1|plot/plant) + (1|year:date), data = balsvolboth)
summary(modlnvol)
plot(modlnvol)
inflvol <- influence(modlnvol, obs = T)
plot(inflvol, which = "cook", main = "Balsam Volume")

lnvol.grid <- ref.grid(modlnvol)
summary(lnvol.grid)

lnvol.treat <- lsmeans(lnvol.grid, "treatment")
pairs(lnvol.treat)

lnvol.year <- lsmeans(lnvol.grid, "year")
pairs(lnvol.year)

int.vol <- pairs(lnvol.grid, by = "year")
int.voltable <- update(int.vol, by = NULL)
int.voltable

test(pairs(int.voltable), joint = T)

Anova(modlnvol, type = 3)

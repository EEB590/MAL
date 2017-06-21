library(ggplot2)
library(lme4)
library(nlme)
library(lubridate)
library(influence.ME)

# Read and minor munging of dataframes

balsam15 <- read.csv("nectar analysis/data files/balsam15.csv", header = T, as.is = T)
balsam16 <- read.csv("nectar analysis/data files/balsam16.csv", header = T, as.is = T)
buckwt15 <- read.csv("nectar analysis/data files/buckwt15.csv", header = T, as.is = T)
buckwt16 <- read.csv("nectar analysis/data files/buckwt16.csv", header = T, as.is = T)

balsam <- rbind(balsam15, balsam16)
buckwt <- rbind(buckwt15, buckwt16)

rm(balsam15)
rm(balsam16)
rm(buckwt15)
rm(buckwt16)

balsam$date <- ymd(balsam$date)
balsam$plot <- as.factor(balsam$plot)
balsam$treatment <- as.factor(balsam$treatment)
balsam$plant <- as.factor(balsam$plant)
balsam$year <- as.factor(year((balsam$date)))


buckwt$date <- ymd(buckwt$date)
buckwt$plot <- as.factor(buckwt$plot)
buckwt$treatment <- as.factor(buckwt$treatment)
buckwt$quad <- as.factor(buckwt$quad)
buckwt$year <- as.factor(year((buckwt$date)))

# Create dataframes for regression analysis

balsam$necpres[balsam$volume != "0"] <- "1"
balsam$necpres[balsam$volume == "0"] <- "0"
balsam$necpres <- as.factor(balsam$necpres)
balsam <- balsam[,-c(5:7)]

buckwt$necpres[buckwt$volume != "0"] <- "1"
buckwt$necpres[buckwt$volume == "0"] <- "0"
buckwt$necpres <- as.factor(buckwt$necpres)
buckwt <- buckwt[,-c(5:7)]

#Data exploration
with(balsam, plot(as.factor(date), necpres, main = "Balsamroot", xlab = "Date", ylab = "Absent (0:dark) / Present (1:light)"))
with(buckwt, plot(as.factor(date), necpres, main = "Buckwheat", xlab = "Date", ylab = "Absent (0:dark) / Present (1:light)"))

with(balsam, plot(plot, necpres, main = "Balsamroot", xlab = "Treatment Plot", ylab = "Absent (0:dark) / Present (1:light)"))
with(buckwt, plot(plot, necpres, main = "Buckwheat", xlab = "Treatment Plot", ylab = "Absent (0:dark) / Present (1:light)"))

#Model diagnostics
  #balsamroot
modbals <- glmer(necpres ~ treatment * year + (1|plot/plant) + (1| year:date), data = balsam, family = binomial)
plot(modbals, main = "Balsam Nectar Presence/Absence")
# inflbals <- influence(modbals, obs = T) - takes a long time, may not converge
# plot(inflbals, which = "cook", main = "Balsam Nectar Presence/Absence")

  #buckwheat
modbuck <- glmer(necpres ~ treatment * year + (1|plot) + (1| year:date), data = buckwt, family = binomial)
plot(modbuck, main = "Buckwheat Nectar Presence/Absence")
# inflbuck <- influence(modbuck, obs = T)
# plot(inflbuck, which = "cook", main = "Buckwheat Nectar Presence/Absence")


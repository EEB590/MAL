library(lme4)
library(lsmeans)
library(ggplot2)
library(blmeco)

setwd("D:/Iowa State University/Debinski Lab/Nectar data/MAL")

#Create df
flowers <- read.csv("nectar analysis/data files/raw data/Balsamroot phenology/TotalFlowersPerPlant.csv", header = T, as.is = T)

flowers$total <- apply(flowers[4:5], 1, sum)
names(flowers)[4:5] <- c("year15", "year16")
flowers$plot <- as.factor(flowers$plot)
flowers$treatment <- as.factor(flowers$treatment)
flowers$plantid <- as.factor(flowers$plantid)
head(flowers)

#Data exploration
qplot(flowers$year15, binwidth = 1, xlab = "# of flowers", main = "2015")
qplot(flowers$year16, binwidth = 1, xlab = "# of flowers", main = "2016")
qplot(flowers$total, binwidth = 1, xlab = "# of flowers", main = "2015 and 2016 totaled")

ggplot(flowers, aes(x = year15)) + geom_histogram(binwidth = 1) + facet_grid(treatment~.)
ggplot(flowers, aes(x = year16)) + geom_histogram(binwidth = 1) + facet_grid(treatment~.)
ggplot(flowers, aes(x = total)) + geom_histogram(binwidth = 1) + facet_grid(treatment~.) +
  labs(title = "Total Flowers by Treatment \n2015 + 2016", subtitle = "2015 + 2016", x = "Total Flowers", y = "Count of Plants")

qplot(flowers$treatment, flowers$year15, geom = "boxplot", xlab = "Treatment", ylab = "# of flowers per plant", main = "2015")
qplot(flowers$treatment, flowers$year16, geom = "boxplot", xlab = "Treatment", ylab = "# of flowers per plant", main = "2016")
qplot(flowers$treatment, flowers$total, geom = "boxplot", xlab = "Treatment", ylab = "# of flowers per plant", main = "2015 and 2016 totaled")

plot(flowers$plot, flowers$year15, main = "2015")
plot(flowers$plot, flowers$year16, main = "2016")
plot(flowers$plot, flowers$total, main = "2015 and 2016 totaled")

# models
  #2015
mod15 <- glmer(year15 ~ treatment + (1|plantid) + (1|plot), data = flowers, family = poisson)
dispersion_glmer(mod15)
summary(mod15)

plot(mod15)

qqnorm(resid(mod15), main="normal qq-plot, residuals")
qqline(resid(mod15))

qqnorm(ranef(mod15)$plantid[,1])
qqline(ranef(mod15)$plantid[,1])

plot(fitted(mod15), jitter(flowers$year15,0.1), xlab = "fitted", ylab = "observed", main = "2015")  #fitted vs observed
abline(0,1)

  #2016
mod16 <- glmer(year16 ~ treatment + (1|plantid) + (1|plot), data = flowers, family = poisson)
dispersion_glmer(mod16)

summary(mod16)

plot(mod16)

qqnorm(resid(mod16), main="normal qq-plot, residuals")
qqline(resid(mod16))

qqnorm(ranef(mod16)$plantid[,1])
qqline(ranef(mod16)$plantid[,1])

plot(fitted(mod16), jitter(flowers$year16,0.1), xlab = "fitted", ylab = "observed", main = "2016")  #fitted vs observed
abline(0,1)



### LSmeans
total15 <- emmeans::emmeans(mod15, c("treatment"), type='response')
emmeans::joint_tests(total15)

total16 <- emmeans::emmeans(mod16, c("treatment"), type='response')
emmeans::joint_tests(total16)


  #2015
summary(mod15)
lnvol.grid <- ref.grid(mod15)
summary(lnvol.grid)
lnvol.treat <- lsmeans(lnvol.grid, "treatment")
pairs(lnvol.treat)
Anova(mod15, type = 3)

  #2016
summary(mod16)
lnvol.grid <- ref.grid(mod16)
summary(lnvol.grid)
lnvol.treat <- lsmeans(lnvol.grid, "treatment")
pairs(lnvol.treat)
Anova(mod16, type = 3)


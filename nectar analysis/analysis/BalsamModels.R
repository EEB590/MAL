library(ggplot2)
library(nlme)

balsvol15 <- read.csv("nectar analysis/data files/balsvol15.csv", header = T)
balsvol16 <- read.csv("nectar analysis/data files/balsvol16.csv", header = T)
balsvolboth <- rbind(balsvol15,balsvol16)


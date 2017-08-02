library(ggplot2)

taxa <- c("Fruit Fly", "Hawkmoth", "Butterfly", "Short-tongue bee", "Stingless bee", "Bumblebee", "Honeybee", "Hummingbird", "Sundbird", "Honeyeater", "Bat")
mins <- (c(24,34,30,50,45,60,40,30,30,40,60))
maxs <- (c(24,34,45,65,60,60,45,30,50,50,60))

conc <- cbind(taxa, mins, maxs)
conc <- as.data.frame(conc, stringsAsFactors = F)
names(conc) <- c("taxa", "min", "max")
conc$min <- as.numeric(conc$min)
conc$max <- as.numeric(conc$max)

bals15 <- read.csv("nectar analysis/data files/balssugar15.csv")
buck15 <- read.csv("nectar analysis/data files/bucksugar15.csv")

meanCbals <- mean(subset(bals15, treatment == "C")$BRIX)
meanHbals <- mean(subset(bals15, treatment == "H")$BRIX)
sdCbals <- sd(subset(bals15, treatment == "C")$BRIX)
sdHbals <- sd(subset(bals15, treatment == "H")$BRIX)

meanCbuck <- mean(subset(buck15, treatment == "C")$BRIX)
meanHbuck <- mean(subset(buck15, treatment == "H")$BRIX)
sdCbuck <- sd(subset(buck15, treatment == "C")$BRIX)
sdHbuck <- sd(subset(buck15, treatment == "H")$BRIX)


ggplot(conc, aes(x = taxa, y = max)) +
  geom_linerange(data = conc[c(3,4,5,7,9,10),], mapping = aes(x = taxa, ymin = min, ymax = max), size = 2) +
  geom_pointrange(data = conc[c(1,2,6,8,11),], mapping = aes(x = taxa, y = max, ymin = min, ymax = max), size = 1) +
  labs(x = "Taxon", y = "Concentration (BRIX)", title = "Optimum Nectar Concentrations") +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
  geom_hline(yintercept = meanHbals, col = "red") +
  geom_hline(yintercept = meanCbals, col = "green") +
  geom_hline(yintercept = meanHbuck, col = "red", linetype = "dashed") +
  geom_hline(yintercept = meanCbuck, col = "green", linetype = "dashed")
  

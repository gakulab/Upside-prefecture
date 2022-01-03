# Analyze output - Tiger Puffer
## Kanae Tokunaga
## Sept 27, 2019


# Load packages
library(ggplot2)
library(tidyr)
source("multiplot.R")

# Read data 
mo <- read.csv("output/tigerPuffer_180927_mo_opt.csv")


mo <- subset(mo, mo$management == "SQ"| mo$management == "FMSY"| mo$management == "econOpt")
moCS <- subset(mo, mo$catchShare == "CS")
mo <- subset(mo, mo$catchShare == "no_CS")


mo.harvest <- mo[,c("fishery","management", "time", "harvest1", "harvest2", "harvest3", "harvest4")]
mo.harvest <- gather(mo.harvest, fleet, harvest, harvest1:harvest4, factor_key = TRUE)

mo.profit <- mo[,c("fishery", "management", "time", "profit1", "profit2", "profit3", "profit4")]
mo.profit <- gather(mo.profit, fleet, profit, profit1:profit4, factor_key = TRUE)

mo.biomass <- mo[,c("fishery", "management", "time", "biomass")]

mo.kobe <- mo[,c("fishery", "management", "time", "BvBMSY", "FvFMSY1","FvFMSY2","FvFMSY3","FvFMSY4")]
mo.kobe <- gather(mo.kobe, fleet, FvFMSY, FvFMSY1:FvFMSY4, factor_key = TRUE)

# Plots
fishery.list <- unique(mo$fishery)
harvest.plot <- list()
for(i in seq_along(fishery.list)){
  harvest.plot[[i]] <- ggplot(subset(mo.harvest, mo.harvest$fishery == fishery.list[i]), 
                         aes(time, harvest, color = management)) + geom_line() + 
    ggtitle(fishery.list[i]) +
    facet_wrap(~ fleet, ncol = 4) +
    theme(legend.position = "none")
}
multiplot(harvest.plot[[2]], harvest.plot[[3]], harvest.plot[[5]], harvest.plot[[4]], harvest.plot[[6]], cols = 1)
multiplot(harvest.plot[[8]], harvest.plot[[9]], harvest.plot[[11]], harvest.plot[[10]], harvest.plot[[12]], cols = 1)


FvFMSY.plot <- list()
for(i in seq_along(fishery.list)){
  FvFMSY.plot[[i]] <- ggplot(subset(mo.kobe, mo.kobe$fishery == fishery.list[i]), 
                              aes(time, FvFMSY, color = management)) + geom_line() + 
    ggtitle(fishery.list[i]) +
    facet_wrap(~ fleet, ncol = 4) +
    theme(legend.position = "none")
}
multiplot(FvFMSY.plot[[2]], FvFMSY.plot[[3]], FvFMSY.plot[[5]], FvFMSY.plot[[4]], FvFMSY.plot[[6]], cols = 1)
multiplot(FvFMSY.plot[[8]], FvFMSY.plot[[9]], FvFMSY.plot[[11]], FvFMSY.plot[[10]], FvFMSY.plot[[12]], cols = 1)


profit.plot <- list()
for(i in seq_along(fishery.list)){
  profit.plot[[i]] <- ggplot(subset(mo.profit, mo.profit$fishery == fishery.list[i]), 
                             aes(time, profit, color = management)) + geom_line() + 
    ggtitle(fishery.list[i]) +
    facet_wrap(~ fleet, ncol = 4) +
    theme(legend.position = "none")
}
multiplot(profit.plot[[2]], profit.plot[[3]], profit.plot[[5]], profit.plot[[4]], profit.plot[[6]], cols = 1)
multiplot(profit.plot[[8]], profit.plot[[9]], profit.plot[[11]], profit.plot[[10]], profit.plot[[12]], cols = 1)

ggplot(mo.biomass, aes(time, biomass, color = management)) + geom_line() + facet_wrap(~ fishery)

# Kobe plot
# Read Ichinokawa data
dt.ichi <- read.csv("data/ichinokawa_180814_puffer.csv")
dt.kobe.sim <- subset(mo, mo$fishery == fishery.list[1])[,c("time", "management", "BvBMSY", "FvFMSYtotal")]
dt.kobe.sim$year <- dt.kobe.sim$time + 2012
dt.kobe.sim <- dt.kobe.sim[,-c(1)]
colnames(dt.kobe.sim) <- c("management", "BvBmsy", "FvFmsy", "year")
dt.kobe.ichi <- dt.ichi[,c("year", "BvBmsy", "FvFmsy")]
dt.kobe.ichi$management <- rep("historical", nrow(dt.kobe.ichi))

dt.kobe <- rbind(dt.kobe.ichi, dt.kobe.sim)
dt.kobe$hist <- as.factor(as.numeric(dt.kobe$management != "historical"))


kobe.plot <- ggplot(dt.kobe, aes(BvBmsy, FvFmsy, color = management)) + geom_point(aes(shape = management), size = 3) +
  #geom_line() +
  #geom_line(aes(year, BvBmsy, group = management))+
  scale_shape_manual(values = c(4, 0, 1, 2))+
  geom_vline(xintercept = 1) + geom_hline(yintercept = 1) +
  #facet_wrap(~hist) +
  geom_text(aes(label=year),hjust=0, vjust=0) +
  theme_bw()

kobe.plot.hist <- ggplot(dt.kobe.ichi, aes(BvBmsy, FvFmsy)) + geom_point(size = 3) + geom 






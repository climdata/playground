library(zoo)
library("ggplot2")
library(gridExtra)
library(grid)
library(qdapTools)

setwd("C:/Code/climData/glaser2019")

#hhi <- read.csv("csv/hhi_summer_1000_2019.csv", sep=",", na = "NA")

#half  <- read.csv("csv/tmb_folgen_5_10.csv", sep=",")

piNew <- read.csv("csv/pm0_4.csv", sep=",")  # 1500
quart <- read.csv("csv/tmb_folgen_6_8.csv", sep=",")
quart <- subset(quart, quart$count>0)
quart <- quart[,c('year','davg', 'dext')]
quart$dmod <- (quart$davg + quart$dext) / 2.0 

piAll <- read.csv("csv/p_1000_2018.csv", sep=",", na = "NA")
piSummer <- rollapply(piAll$precipitation, width=3, by=1, FUN=sum)
piAll$piSummer <- piAll$precipitation
piAll$piSummer[3:length(piAll$precipitation)] <- piSummer/3.0
piAllSummer <- subset(piAll, piAll$month==8)

piOld <- subset(piAllSummer, piAllSummer$year<1500)
piOld <- piOld[,c('year', 'piSummer')]


old <- merge(quart,piOld, by=c("year"))
old <- old[,c('davg', 'dext', 'dmod', 'piSummer')]
new <- merge(quart,piNew, by=c("year")) 
new <- new[,c('davg', 'dext', 'dmod', 'pavg', 'pext', 'pm2')]

library(psych)
pairs.panels(new)
pairs.panels(old)



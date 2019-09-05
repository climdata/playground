#install.packages("ggplot2")
#install.packages("RColorBrewer")
#install.packages("zoo")

library("RColorBrewer")
require("ggplot2")
library(zoo)

par(mfrow=c(1,1))

setwd("C:/Code/climData/glaser2010")


p0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/p_1500_1995.csv", sep=",", na = "NA")
spifull <- read.csv("https://raw.githubusercontent.com/climdata/dwdSPI/master/csv/spi_de.csv", sep=",", na = "NA")

p1 <- data.frame()
monthNames = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for(i in 1:length(monthNames)) {
  pnew <- data.frame(year = p0$year, month = i)
  pnew$prec0 <- p0[,monthNames[i]]
  p1 <- rbind(p1, pnew)
}

p1$time = p1$year + (p1$month+0.5)/12.0

spinew <- subset(spifull, spifull$time>max(p1$time))
spinew <- spinew[, c("year","month","time","spi1")]
names(spinew)[names(spinew) == 'spi1'] <- 'prec0'
spinew <- spinew[order(spinew$time),]

### DOES NOT WORK !!!
for(i in length(spinew$prec0)) {
  if(spinew$prec0[i] > 3.0) {
    spinew$prec0[i] = 3.0
  }
  if(spinew$prec0[i] < -3.0) {
    spinew$prec0[i] = -3.0
  }  
}

p1 <- rbind(p1, spinew)
p1 <- p1[order(p1$time),]


mp <- ggplot(p1, aes(year, month))
mp + geom_raster(aes(fill=prec0))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30",
                       limits=c(-3,3)) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="PI", reverse = TRUE))  
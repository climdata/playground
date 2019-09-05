library(zoo)
library("ggplot2")
library(gridExtra)
library(grid)
library(qdapTools)

setwd("C:/Code/climData/glaser2019")

spiFull <- read.csv("https://raw.githubusercontent.com/climdata/dwdSPI/master/csv/spi_de.csv", sep=",", na = "NA")
spiCal <- subset(spifull, spiFull$year>1880 & spiFull$year<1995)

hiFull <- read.csv("csv/spi_1500_2019.csv", sep=",", na = "NA")
hiFull <- subset(hiFull, hiFull$year>1499)




spiCal$di1 <- spiCal$spi1
spiCal$di2 <- spiCal$spi2
spiCal$di3 <- spiCal$spi3
spiCal$di4 <- spiCal$spi4
spiCal$di5 <- spiCal$spi5

spiCal$di <- spiCal$spi12 

for(i in 1:nrow(spiCal)) {
  spiCal$di1[i] <- max(-1.0,min(spiCal$spi1[i], spiCal$spi2[i], 0.0, na.rm = TRUE))

  spiCal$di2[i] <- max(-1.5,min(spiCal$spi1[i], spiCal$spi2[i], 1.0, na.rm = TRUE))
  if (spiCal$di2[i]>-1.0) {spiCal$di2[i] = 0.0}

  spiCal$di3[i] <- max(-2.0,min(spiCal$spi2[i], spiCal$spi3[i], spiCal$spi4[i], 1.0, na.rm = TRUE))
  if (spiCal$di3[i]>-1.5) {spiCal$di3[i] = 0.0}  

  spiCal$di4[i] <- max(-3.0,min(spiCal$spi4[i], spiCal$spi5[i], spiCal$spi6[i],spiCal$spi7[i], spiCal$spi8[i], spiCal$spi9[i],spiCal$spi10[i], 1.0, na.rm = TRUE))
  if (spiCal$di4[i]>-2.0) {spiCal$di4[i] = 0.0}
  
  spiCal$di5[i] <- max(-4.0,min(spiCal$spi10[i], spiCal$spi11[i], spiCal$spi12[i], 1.0, na.rm = TRUE))
  if (spiCal$di5[i]>-3.0) {spiCal$di5[i] = 0.0}   

  spiCal$di[i] <- min(spiCal$di1[i],spiCal$di2[i],spiCal$di3[i],spiCal$di4[i],spiCal$di5[i], 0.0, na.rm = TRUE)
  }

## HDI

hiFull$di1 <- hiFull$spi1
hiFull$di2 <- hiFull$spi2
hiFull$di3 <- hiFull$spi3
hiFull$di4 <- hiFull$spi4
hiFull$di5 <- hiFull$spi5

hiFull$di <- hiFull$spi12 

for(i in 1:nrow(hiFull)) {
  hiFull$di1[i] <- max(-1.0,min(hiFull$spi1[i], hiFull$spi2[i], 0.0, na.rm = TRUE))
  
  hiFull$di2[i] <- max(-1.5,min(hiFull$spi1[i], hiFull$spi2[i], 1.0, na.rm = TRUE))
  if (hiFull$di2[i]>-1.0) {hiFull$di2[i] = 0.0}
  
  hiFull$di3[i] <- max(-2.0,min(hiFull$spi2[i], hiFull$spi3[i], hiFull$spi4[i], 1.0, na.rm = TRUE))
  if (hiFull$di3[i]>-1.5) {hiFull$di3[i] = 0.0}  
  
  hiFull$di4[i] <- max(-3.0,min(hiFull$spi4[i], hiFull$spi5[i], hiFull$spi6[i],hiFull$spi7[i], hiFull$spi8[i], hiFull$spi9[i],hiFull$spi10[i], 1.0, na.rm = TRUE))
  if (hiFull$di4[i]>-2.0) {hiFull$di4[i] = 0.0}
  
  hiFull$di5[i] <- max(-4.0,min(hiFull$spi10[i], hiFull$spi11[i], hiFull$spi12[i], 1.0, na.rm = TRUE))
  if (hiFull$di5[i]>-3.0) {hiFull$di5[i] = 0.0}   
  
  hiFull$di[i] <- min(hiFull$di1[i],hiFull$di2[i],hiFull$di3[i],hiFull$di4[i],hiFull$di5[i], 0.0, na.rm = TRUE)
}


hiCal <-  subset(hiFull, hiFull$year>1880 & hiFull$year<1995) 

df <- data.frame(y = hiCal[,di], x = spiCal[,di])
df <- subset(df, !is.na(df$x))
mx <- lm(y ~ x, df);  
eq <- substitute(italic(r)^2~"="~r2, 
                 list(r2 = format(summary(mx)$r.squared, digits = 3)))
eq <- as.character(as.expression(eq))


mp1 <- ggplot() +
  theme_classic(base_size=80) +
  #coord_cartesian(ylim=c(-4,4)) +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-4,4)) +
  labs(x="Year", y="DI", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=-spiCal$di, x=spiCal$time, color="MDI"), size=2) +
  geom_line(aes(y=hiCal$di, x=hiCal$time, color="HDI"), size=2) +
  annotate('text', x = 1990, y = 3.0, label = eq, parse = TRUE, size=20) +
  scale_color_manual(
    name = "", 
    labels = c("MDI+", "HDI-"),
    breaks  = c("MDI", "HDI"),
    values = c("MDI"="#33ccff", "HDI"="#ff9933", "black"="#000000")
  ) + 
  theme( legend.key.width = unit(2,"cm")) 
mp1

## HWI

hiFull$wi1 <- hiFull$spi1
hiFull$wi2 <- hiFull$spi2
hiFull$wi3 <- hiFull$spi3
hiFull$wi4 <- hiFull$spi4
hiFull$wi5 <- hiFull$spi5

hiFull$wi <- hiFull$spi12  
hiFull$hi <- hiFull$spi12  ## Humidity Index => Difference
hiFull$xi <- hiFull$spi12  ## Extreme Index  => Larger Amplitude

for(i in 1:nrow(hiFull)) {
  hiFull$wi1[i] <- min(1.0,max(hiFull$spi1[i], hiFull$spi2[i], 0.0, na.rm = TRUE))
  
  hiFull$wi2[i] <- min(1.5,max(hiFull$spi1[i], hiFull$spi2[i], -1.0, na.rm = TRUE))
  if (hiFull$wi2[i]<1.0) {hiFull$wi2[i] = 0.0}
  
  hiFull$wi3[i] <- min(2.0,max(hiFull$spi2[i], hiFull$spi3[i], hiFull$spi4[i], -1.0, na.rm = TRUE))
  if (hiFull$wi3[i]<1.5) {hiFull$wi3[i] = 0.0}  
  
  hiFull$wi4[i] <- min(3.0,max(hiFull$spi4[i], hiFull$spi5[i], hiFull$spi6[i],hiFull$spi7[i], hiFull$spi8[i], hiFull$spi9[i],hiFull$spi10[i], -1.0, na.rm = TRUE))
  if (hiFull$wi4[i]<2.0) {hiFull$wi4[i] = 0.0}
  
  hiFull$wi5[i] <- min(4.0,max(hiFull$spi10[i], hiFull$spi11[i], hiFull$spi12[i], -1.0, na.rm = TRUE))
  if (hiFull$wi5[i]<3.0) {hiFull$wi5[i] = 0.0}   
  
  hiFull$wi[i] <- max(hiFull$wi1[i],hiFull$wi2[i],hiFull$wi3[i],hiFull$wi4[i],hiFull$wi5[i], 0.0, na.rm = TRUE)
  hiFull$hi[i] <- hiFull$wi[i] + hiFull$di[i]
  if (hiFull$hi[i] < 0) {
    hiFull$xi[i] <- hiFull$di[i]
  } else {
    hiFull$xi[i] <- hiFull$wi[i]
  }
  
}

mp2 <- ggplot() +
  theme_classic(base_size=80) +
  #coord_cartesian(ylim=c(-4,4)) +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-4,4)) +
  labs(x="Year", y="HDI       HWI", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=hiFull$wi5, x=hiFull$time, color="a:HWI05"), size=1) +
  geom_line(aes(y=hiFull$wi4, x=hiFull$time, color="b:HWI04"), size=1.5) +
  geom_line(aes(y=hiFull$wi3, x=hiFull$time, color="c:HWI03"), size=2) +
  geom_line(aes(y=hiFull$wi2, x=hiFull$time, color="d:HWI02"), size=2.5) +
  geom_line(aes(y=hiFull$wi1, x=hiFull$time, color="e:HWI01"), size=3) +
  geom_line(aes(y=hiFull$di5, x=hiFull$time, color="j:HDI05"), size=1) +  
  geom_line(aes(y=hiFull$di4, x=hiFull$time, color="i:HDI04"), size=1.5) + 
  geom_line(aes(y=hiFull$di3, x=hiFull$time, color="h:HDI03"), size=2) +  
  geom_line(aes(y=hiFull$di2, x=hiFull$time, color="g:HDI02"), size=2.5) +  
  geom_line(aes(y=hiFull$di1, x=hiFull$time, color="f:HDI01"), size=3) +
  scale_color_manual(
    name = "", 
    labels = c("HWI5", "HWI4", "HWI3", "HWI2", "HWI1",
               "HDI1", "HDI2", "HDI3", "HDI4", "HDI5"),
    breaks  = c("a:HWI05", "b:HWI04", "c:HWI03", "d:HWI02", "e:HWI01",
                "f:HDI01", "g:HDI02", "h:HDI03", "i:HDI04", "j:HDI05"),
    values = c("a:HWI05"="#005500",
               "b:HWI04"="#009933",               
               "c:HWI03"="#00ff00",               
               "d:HWI02"="#66ff99",               
               "e:HWI01"="#99ff99",
               "f:HDI01"="#ffffcc", 
               "g:HDI02"="#ffff11",                
               "h:HDI03"="#ff9900", 
               "i:HDI04"="#ff0000",                
               "j:HDI05"="#990000"                
               )
  ) + 
  theme( legend.key.width = unit(2,"cm")) 
mp2



custom.col <- c("#CC0000", "#FF0000", "#FF6600", "#FFFF00", "#FFEEBB",
                "#FFFFFF",
                "#CCFFCC", "#99EE99", "#00cc00", "#339933", "#004400")

custom.col2 <- c("#CC0000", "#FF0000", "#FF6600", "#FFFF00", "#FFEEBB", "#FFFFFF")

custom.col3 <- c("#FFFFFF", "#CCFFCC", "#99EE99", "#00cc00", "#339933", "#004400")


mp <- ggplot(hiFull, aes(year, month))

mp + geom_raster(aes(fill=hi))+
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_fill_gradientn(colours=custom.col) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

mp + geom_raster(aes(fill=xi))+
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_fill_gradientn(colours=custom.col) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

mp + geom_raster(aes(fill=di))+
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_fill_gradientn(colours=custom.col2) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HDI", reverse = TRUE))

mp + geom_raster(aes(fill=wi))+
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_fill_gradientn(colours=custom.col3) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HWI", reverse = TRUE))


hhi <- hiFull[,c('year','month','time', 'di', 'wi', 'hi', 'xi')]
names(hhi)[names(hhi) == 'di'] <- 'hdi'
hhi$hdi <- round(hhi$hdi, digits=6)
names(hhi)[names(hhi) == 'wi'] <- 'hwi'
hhi$hwi <- round(hhi$hwi, digits=6)
names(hhi)[names(hhi) == 'hi'] <- 'hhi'
hhi$hhi <- round(hhi$hhi, digits=6)
names(hhi)[names(hhi) == 'xi'] <- 'hxi'
hhi$hxi <- round(hhi$hxi, digits=6)

write.table(hhi, file = "csv/hhi_1500_2019.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "escape", fileEncoding = "UTF-8")






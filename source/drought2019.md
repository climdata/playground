---
title: "Drought 2019"
author: "Kmicha71"
date: "6 8 2019"
output:
  html_document: 
    keep_md: true
  pdf_document: default
---



# Reconstructions of Droughts in Germany since 1500

[![DOI](https://www.zenodo.org/badge/206506760.svg)](https://www.zenodo.org/badge/latestdoi/206506760)

[DOI:10.5281/zenodo.3405167](https://doi.org/10.5281/zenodo.3405167)

[DOI:10.5194/cp-2019-104](https://doi.org/10.5194/cp-2019-104)

The present code deals with the reconstruction of drought time series in Germany since 1500. The reconstructions are based on written records from the historical climate and environmental database tambora.org, early and official instrument data as well as precipitation and temperature indices.

## Visualize tambora.org data related to droughts



```r
require("ggplot2")
```

```
## Loading required package: ggplot2
```

```r
require("extrafont")
```

```
## Loading required package: extrafont
```

```
## Registering fonts with R
```

```r
require("ggmap")
```

```
## Loading required package: ggmap
```

```
## Google's Terms of Service: https://cloud.google.com/maps-platform/terms/.
```

```
## Please cite ggmap if you use it! See citation("ggmap") for details.
```

```r
#require("rnaturalearth")
#require("rnaturalearthdata")
#require("sf")
#require("scales")
```

## Load and Visialize Monthly Precipitation Data (PI)



```r
t0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/ti_1500_2xxx_monthly.csv", sep=",", na = "NA")
t1 <- t0[,c("year","month","ti")]
p0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/pi_1500_2xxx_monthly.csv", sep=",", na = "NA")
p2 <- p0[,c("year","month","pi")]
spifull <- read.csv("https://raw.githubusercontent.com/climdata/dwdSPI/master/csv/spi_de.csv", sep=",", na = "NA")
#p0 <- distinct(p0, year,month, .keep_all= TRUE)
spinew <- subset(spifull, spifull$ts>max(p0$ts))
spinew <- spinew[, c("year","month","ts","time","spi1")]
names(spinew)[names(spinew) == 'spi1'] <- 'pi'
spinew <- spinew[order(spinew$ts),]
### Limit range
for(i in length(spinew$pi)) {
  if(spinew$pi[i] > 3.0) {
    spinew$pi[i] = 3.0
  }
  if(spinew$pi[i] < -3.0) {
    spinew$pi[i] = -3.0
  }  
}
p1 <- rbind(p0, spinew)
p1 <- p1[order(p1$ts),]
mp <- ggplot(p1, aes(year, month))
mp + geom_raster(aes(fill=pi))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30",
                       limits=c(-3,3)) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="PI", reverse = TRUE))  
```

![](drought2019_files/figure-html/pi-1.png)<!-- -->


## Calibration Historical Precipitation Index (HPI) vs Standard Precipitation Index (SPI)


```r
library(zoo)
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```r
library("ggplot2")
library(gridExtra)
library(grid)
library(qdapTools)
spiCal <- subset(spifull, spifull$year>1880 & spifull$year<1996)
spiCal <- spiCal[order(spiCal$ts),]

tps <- merge(spifull,p2, by=c("year","month"))
tps <- merge(tps,t1, by=c("year","month"))
tps$t1 <- sin(tps$month*pi/6)
tps$t2 <- cos(tps$month*pi/6)
tps <- tps[order(tps$ts),]

mx <- lm(spi1 ~ (pi+ti+t1+t2)^2, tps)
#mx <- lm(spi1 ~ pi, tps)
summary(mx)
```

```
## 
## Call:
## lm(formula = spi1 ~ (pi + ti + t1 + t2)^2, data = tps)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -2.47032 -0.34117  0.03216  0.36628  2.17180 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -0.018325   0.015259  -1.201 0.229989    
## pi           0.570965   0.011620  49.137  < 2e-16 ***
## ti          -0.054445   0.014209  -3.832 0.000133 ***
## t1           0.080588   0.021736   3.708 0.000218 ***
## t2          -0.050919   0.021987  -2.316 0.020711 *  
## pi:ti        0.023094   0.011055   2.089 0.036897 *  
## pi:t1       -0.008405   0.016369  -0.513 0.607711    
## pi:t2        0.018960   0.016294   1.164 0.244775    
## ti:t1        0.099162   0.019602   5.059 4.79e-07 ***
## ti:t2        0.219818   0.020092  10.941  < 2e-16 ***
## t1:t2        0.186374   0.043185   4.316 1.71e-05 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5613 on 1369 degrees of freedom
## Multiple R-squared:  0.6856,	Adjusted R-squared:  0.6833 
## F-statistic: 298.5 on 10 and 1369 DF,  p-value: < 2.2e-16
```

```r
tps1 <- merge(p1,t1, by=c("year","month"))
tps1$t1 <- sin(tps1$month*pi/6)
tps1$t2 <- cos(tps1$month*pi/6)
tps1$ts <- signif(tps1$year + (tps1$month-0.5)/12, digits=6)
tps1$time <- paste(tps1$year,tps1$month, '15 00:00:00', sep='-')
tps1 <- tps1[order(tps1$ts),]

pr <- predict(mx, newdata=tps1, se.fit=TRUE) 

tps1$hdi1 <- pr$fit
tps1$hdi_se1 <- pr$se



hi <- tps1
#hi$hdi1 <- hi$pi
hi <- hi[order(hi$ts),]
prev <- hi$hdi1
for (m in c(2,3,4,5,6,7,8,9,10,11,12)) {
  column <- paste("hdi", m, sep="")
  hdi <- rollapply(hi$hdi1, width=m, by=1, FUN=sum)
  hi$hdi <- prev
  hi$hdi[m:length(hi$hdi)] <- hdi
  prev <- hi$hdi
  names(hi)[names(hi) == 'hdi'] <- column
}
hiCal <- subset(hi, hi$year>1880 & hi$year<1996)
hdispi <- merge(hiCal,spiCal, by=c("year","month"))

plots <-  list()
slopes <- list()
offsets <- list()
#par(mfrow=c(3,4))
for (m in c(1:12)) {
  yBreaks <-c(-15,-10,-5,0,5,10,15)
  if(m==1) {
    yBreaks <-c(-3,-2,-1,0,1,2,3)
  }
  hdiCol <- paste("hdi", m, sep="")
  spiCol <- paste("spi", m, sep="")
  df <- data.frame(y = hdispi[,hdiCol], x = hdispi[,spiCol])
  df <- subset(df, !is.na(df$x))
  mx <- lm(y ~ x, df);  
  slope <- unname(coef(mx)[2])
  offset <- unname(coef(mx)[1])
  
  predx <- seq(-3, +3, len=50)
  pred <- predict(mx, newdata=list(x=predx), se.fit=TRUE)  
  
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(mx)[1]), digits = 3),
                        b = format(unname(coef(mx)[2]), digits = 5),
                        r2 = format(summary(mx)$r.squared, digits = 3)))
  eq <- as.character(as.expression(eq))
  p <- ggplot(data = df, aes(x = x, y = y)) +
    theme_classic(base_size=60) +
    #theme_classic(base_size=5) +
    scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-3.5,3.5)) +
    scale_y_continuous(breaks=yBreaks) +
    geom_hline(aes(yintercept=0)) +
    geom_vline(aes(xintercept=0)) +
    labs(x=toupper(spiCol), y=toupper(hdiCol), title="", subtitle="") +
    geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) +
    geom_point(color="#0000AA", alpha=0.3, size=5) +
    geom_text(x = 1.4, y = -3*slope, label = eq, parse = TRUE, size=15)
    #geom_point(color="#0000AA", alpha=0.3, size=0.3) +
    #geom_text(x = 1.0, y = -3*slope, label = eq, parse = TRUE, size=2)
  plots[[length(plots) + 1]] <- p
  slopes[[length(slopes) + 1]] <- slope
  offsets[[length(offsets)+ 1]] <- offset
}
margin = theme(plot.margin = unit(c(1,1,1,1), "mm"))
p <- grid.arrange(grobs = lapply(plots, "+", margin), nrow=4)
```

```
## Warning: Removed 3 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 3 rows containing missing values (geom_point).
```

```
## Warning: Removed 4 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 4 rows containing missing values (geom_point).
```

```
## Warning: Removed 2 rows containing non-finite values (stat_smooth).
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

![](drought2019_files/figure-html/spiCalib1-1.png)<!-- -->

```r
##plots[11]
```
## Duration and scale factors of the relationship between SPI and HPI


```r
df <- list2df(slopes, col1="y")
df$x <- c(1:12)
mx <- lm(log(df$y) ~ log(df$x), df)
pred <- predict(mx, se.fit=TRUE)
aSlope <- exp(unname(coef(mx)[1]))
bSlope <- unname(coef(mx)[2])
eq <- substitute(italic(slope) == a %.% italic(months)^b*","~~italic(r)^2~"="~r2, 
                 list(a = format(exp(unname(coef(mx)[1])), digits = 4),
                      b = format(unname(coef(mx)[2]), digits = 4),
                      r2 = format(summary(mx)$r.squared, digits = 4)))
eq <- as.character(as.expression(eq))
p <- ggplot(data = df, aes(x = x, y = y)) +
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Months", y="Slope", title="", subtitle="") +
  geom_path(x=df$x, y=exp(pred$fit+2*pred$se.fit), color='#0088bb', lwd=2) +
  geom_path(x=df$x, y=exp(pred$fit-2*pred$se.fit), color='#0088bb', lwd=2) +
  geom_path(x=df$x, y=exp(pred$fit+0*pred$se.fit), color='#0000AA', lwd=3.5) +
  geom_point(color="#0000AA", size=25) + geom_text(x = 8, y = 2, label = eq, parse = TRUE, size=50)
  #geom_point(color="#0000AA", size=2) + geom_text(x = 8, y = 2, label = eq, parse = TRUE, size=5)
p
```

![](drought2019_files/figure-html/spiCalib2-1.png)<!-- -->


## Duration and scale factors of the relationship between SPI and HPI


```r
df <- list2df(offsets, col1="y")
df$x <- c(1:12)
mx <- lm(df$y ~ df$x, df)
pred <- predict(mx, se.fit=TRUE)
bOffset <- unname(coef(mx)[2])
eq <- substitute(italic(offset) == a + b %.% italic(months)*","~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(mx)[1]), digits = 4),
                      b = format(unname(coef(mx)[2]), digits = 4),
                      r2 = format(summary(mx)$r.squared, digits = 4)))
eq <- as.character(as.expression(eq))
p <- ggplot(data = df, aes(x = x, y = y)) +
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Months", y="Slope", title="", subtitle="") +
  geom_path(x=df$x, y=pred$fit+2*pred$se.fit, color='#0088bb', lwd=2) +
  geom_path(x=df$x, y=pred$fit-2*pred$se.fit, color='#0088bb', lwd=2) +
  geom_path(x=df$x, y=pred$fit+0*pred$se.fit, color='#0000AA', lwd=3.5) +
  geom_point(color="#0000AA", size=25) + geom_text(x = 8, y = 2, label = eq, parse = TRUE, size=50)
  #geom_point(color="#0000AA", size=2) + geom_text(x = 8, y = 0.05, label = eq, parse = TRUE, size=5)
p
```

![](drought2019_files/figure-html/spiCalib3-1.png)<!-- -->


## Calculate SPI from HPI for 1500-xxxx


```r
hspi <- hi[,c('year','month','time', 'ts')]
for (m in c(1:12)) {
  hdiCol <- paste("hdi", m, sep="")
  spiCol <- paste("spi", m, sep="")
  spi <- hi[,hdiCol] * m^(-1/sqrt(3.0))
  # more exact reconstruction
  spi <- (hi[,hdiCol] - bOffset * m)  / (aSlope * m^bSlope)
  hspi$spi <- round(spi, digits=6)
  names(hspi)[names(hspi) == 'spi'] <- spiCol
}
## Add rezent spi data 
spinew <- subset(spifull, spifull$ts>max(hspi$ts))
hspi <- rbind(hspi, spinew)
write.table(hspi, file = "csv/spi_1500_2xxx.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "escape", fileEncoding = "UTF-8")
```


## Plot SPI for 1500-2xxx


```r
mp1 <- ggplot(hspi, aes()) +
  theme_classic(base_size=80) +
  #theme_classic() +
  #coord_cartesian(ylim=c(0,300)) +
  labs(x="Year", y="SPI", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=hspi$spi3, x=hspi$ts, color="SPI03"), size=2.5) +
  geom_line(aes(y=hspi$spi6, x=hspi$ts, color="SPI06"), size=2.5) +
  geom_line(aes(y=hspi$spi12, x=hspi$ts, color="SPI12"), size=2.5) +
  scale_color_manual(
    name = "", 
    labels = c("SPI3", "SPI6", "SPI12"),
    breaks  = c("SPI03", "SPI06", "SPI12"),
    values = c("SPI03"="#33ddff", "SPI06"="#0088bb","SPI12"="#003344")
  ) + 
  #guides(linetype = guide_legend(override.aes = list(size = 6))) +
  theme( legend.key.width = unit(2,"cm")) 
mp1
```

![](drought2019_files/figure-html/spiPlot-1.png)<!-- -->

## Calculate Historical Drought Index(HDI) and Modern Drought Index (MDI)


```r
hiFull <- hspi
## Modern Drought Index MDI
spiCal$mdi1 <- spiCal$spi1
spiCal$mdi2 <- spiCal$spi2
spiCal$mdi3 <- spiCal$spi3
spiCal$mdi4 <- spiCal$spi4
spiCal$mdi5 <- spiCal$spi5
spiCal$mdi <- spiCal$spi12 
for(i in 1:nrow(spiCal)) {
  spiCal$mdi1[i] <- max(-1.0,min(spiCal$spi1[i], spiCal$spi2[i], 0.0, na.rm = TRUE))
  spiCal$mdi2[i] <- max(-1.5,min(spiCal$spi1[i], spiCal$spi2[i], 1.0, na.rm = TRUE))
  if (spiCal$mdi2[i]>-1.0) {spiCal$mdi2[i] = 0.0}
  spiCal$mdi3[i] <- max(-2.0,min(spiCal$spi2[i], spiCal$spi3[i], spiCal$spi4[i], 1.0, na.rm = TRUE))
  if (spiCal$mdi3[i]>-1.5) {spiCal$mdi3[i] = 0.0}  
  spiCal$mdi4[i] <- max(-3.0,min(spiCal$spi4[i], spiCal$spi5[i], spiCal$spi6[i],spiCal$spi7[i], spiCal$spi8[i], spiCal$spi9[i],spiCal$spi10[i], 1.0, na.rm = TRUE))
  if (spiCal$mdi4[i]>-2.0) {spiCal$mdi4[i] = 0.0}
  
  spiCal$mdi5[i] <- max(-4.0,min(spiCal$spi10[i], spiCal$spi11[i], spiCal$spi12[i], 1.0, na.rm = TRUE))
  if (spiCal$mdi5[i]>-3.0) {spiCal$mdi5[i] = 0.0}   
  spiCal$mdi[i] <- min(spiCal$mdi1[i],spiCal$mdi2[i],spiCal$mdi3[i],spiCal$mdi4[i],spiCal$mdi5[i], 0.0, na.rm = TRUE)
}
## Historical Drought Index HDI
hiFull$hdi1 <- hiFull$spi1
hiFull$hdi2 <- hiFull$spi2
hiFull$hdi3 <- hiFull$spi3
hiFull$hdi4 <- hiFull$spi4
hiFull$hdi5 <- hiFull$spi5
hiFull$hdi <- hiFull$spi12 
for(i in 1:nrow(hiFull)) {
  hiFull$hdi1[i] <- max(-1.0,min(hiFull$spi1[i], hiFull$spi2[i], 0.0, na.rm = TRUE))
  
  hiFull$hdi2[i] <- max(-1.5,min(hiFull$spi1[i], hiFull$spi2[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi2[i]>-1.0) {hiFull$hdi2[i] = 0.0}
  
  hiFull$hdi3[i] <- max(-2.0,min(hiFull$spi2[i], hiFull$spi3[i], hiFull$spi4[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi3[i]>-1.5) {hiFull$hdi3[i] = 0.0}  
  
  hiFull$hdi4[i] <- max(-3.0,min(hiFull$spi4[i], hiFull$spi5[i], hiFull$spi6[i],hiFull$spi7[i], hiFull$spi8[i], hiFull$spi9[i],hiFull$spi10[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi4[i]>-2.0) {hiFull$hdi4[i] = 0.0}
  
  hiFull$hdi5[i] <- max(-4.0,min(hiFull$spi10[i], hiFull$spi11[i], hiFull$spi12[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi5[i]>-3.0) {hiFull$hdi5[i] = 0.0}   
  
  hiFull$hdi[i] <- min(hiFull$hdi1[i],hiFull$hdi2[i],hiFull$hdi3[i],hiFull$hdi4[i],hiFull$hdi5[i], 0.0, na.rm = TRUE)
}
## Historical Wet Index HWI
hiFull$hwi1 <- hiFull$spi1
hiFull$hwi2 <- hiFull$spi2
hiFull$hwi3 <- hiFull$spi3
hiFull$hwi4 <- hiFull$spi4
hiFull$hwi5 <- hiFull$spi5
hiFull$hwi <- hiFull$spi12  
hiFull$hdwi <- hiFull$spi12  ## Drought/Wet Index => Difference
hiFull$hhi <- hiFull$spi12  ## Humidity Index  => Maximum Extreme -> Larger Amplitude
for(i in 1:nrow(hiFull)) {
  hiFull$hwi1[i] <- min(1.0,max(hiFull$spi1[i], hiFull$spi2[i], 0.0, na.rm = TRUE))
  
  hiFull$hwi2[i] <- min(1.5,max(hiFull$spi1[i], hiFull$spi2[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi2[i]<1.0) {hiFull$hwi2[i] = 0.0}
  
  hiFull$hwi3[i] <- min(2.0,max(hiFull$spi2[i], hiFull$spi3[i], hiFull$spi4[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi3[i]<1.5) {hiFull$hwi3[i] = 0.0}  
  
  hiFull$hwi4[i] <- min(3.0,max(hiFull$spi4[i], hiFull$spi5[i], hiFull$spi6[i],hiFull$spi7[i], hiFull$spi8[i], hiFull$spi9[i],hiFull$spi10[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi4[i]<2.0) {hiFull$hwi4[i] = 0.0}
  
  hiFull$hwi5[i] <- min(4.0,max(hiFull$spi10[i], hiFull$spi11[i], hiFull$spi12[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi5[i]<3.0) {hiFull$hwi5[i] = 0.0}   
  
  hiFull$hwi[i] <- max(hiFull$hwi1[i],hiFull$hwi2[i],hiFull$hwi3[i],hiFull$hwi4[i],hiFull$hwi5[i], 0.0, na.rm = TRUE)
  hiFull$hdwi[i] <- hiFull$hwi[i] + hiFull$hdi[i]
  if (hiFull$hdwi[i] < 0) {
    hiFull$hhi[i] <- hiFull$hdi[i]
  } else {
    hiFull$hhi[i] <- hiFull$hwi[i]
  }
  
}
hhi <- hiFull[,c('year','month','time', 'ts', 'hdi', 'hwi', 'hhi', 'hdwi')]
hhi$hdi <- round(hhi$hdi, digits=6)
hhi$hwi <- round(hhi$hwi, digits=6)
hhi$hhi <- round(hhi$hhi, digits=6)
hhi$hdwi <- round(hhi$hdwi, digits=6)
write.table(hhi, file = "csv/hhi_1500_2xxx.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "escape", fileEncoding = "UTF-8")
```

## Plot HDI/MDI Calibration


```r
hiCal <- subset(hiFull, hiFull$year>1880 & hiFull$year<1996)
hiCal <- hiCal[order(hiCal$ts),]
df <- data.frame(y = hiCal$hdi, x = spiCal$mdi)
df <- subset(df, !is.na(df$x))
mx <- lm(y ~ x, df);  
eq <- substitute(italic(r)^2~"="~r2, 
                 list(r2 = format(summary(mx)$r.squared, digits = 3)))
eq <- as.character(as.expression(eq))
mp1 <- ggplot() +
  theme_classic(base_size=80) +
  #theme_classic() +
  #coord_cartesian(ylim=c(-4,4)) +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-4,4)) +
  labs(x="Year", y="DI", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=-spiCal$mdi, x=spiCal$ts, color="MDI"), size=2.5) +
  geom_line(aes(y=hiCal$hdi, x=hiCal$ts, color="HDI"), size=2.5) +
  annotate('text', x = 1990, y = 3.0, label = eq, parse = TRUE, size=20) +
  scale_color_manual(
    name = "", 
    labels = c("MDI+", "HDI-"),
    breaks  = c("MDI", "HDI"),
    values = c("MDI"="#33ccff", "HDI"="#ff9933", "black"="#000000")
  ) + 
  theme( legend.key.width = unit(2,"cm")) 
mp1
```

![](drought2019_files/figure-html/hdiPlotCal-1.png)<!-- -->

## Calculate Lowpass FFT of HDI & MDI


```r
hiCal2 <- subset(hiFull, hiFull$year>1880 & hiFull$year<1996)
hiCal2 <- hiCal[order(hiCal$ts),]
spiCal2 <- spiCal
#FFT for HDI
pic <- hiCal2$hdi
len <- length(pic)
## mirror pic
pic <- append(pic, pic)
for(i in 1:len) {
  pic[i+len] <- pic[1+len-i]
}
frq <- fft(pic, inverse = FALSE)
frq1 <- frq
filterYears = 1.0   #filter 1y
start = round(len/(12*filterYears))
stop  = round(2*len-start)
frq1[start:stop] <- 0.0 
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
hiCal2$hdi <- pic1
## FFt for MDI
pic <- spiCal2$mdi
len <- length(pic)
## mirror pic
pic <- append(pic, pic)
for(i in 1:len) {
  pic[i+len] <- pic[1+len-i]
}
frq <- fft(pic, inverse = FALSE)
frq1 <- frq
##filterYears = 1.0   #filter 1y
start = round(len/(12*filterYears))
stop  = round(2*len-start)
frq1[start:stop] <- 0.0 
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
spiCal2$mdi <- pic1
```

## Plot HDI & MDI (1y)


```r
df <- data.frame(y = hiCal2$hdi, x = spiCal2$mdi)
df <- subset(df, !is.na(df$x))
mx <- lm(y ~ x, df);  
eq <- substitute(italic(r)^2~"="~r2, 
                 list(r2 = format(summary(mx)$r.squared, digits = 3)))
eq <- as.character(as.expression(eq))
mp1 <- ggplot() +
  theme_classic(base_size=80) +
  #theme_classic() +
  #coord_cartesian(ylim=c(-4,4)) +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-4,4)) +
  labs(x="Year", y="DI (1y)", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=-spiCal2$mdi, x=spiCal2$ts, color="MDI"), size=2.5) +
  geom_line(aes(y=hiCal2$hdi, x=hiCal2$ts, color="HDI"), size=2.5) +
  annotate('text', x = 1990, y = 3.0, label = eq, parse = TRUE, size=20) +
  scale_color_manual(
    name = "", 
    labels = c("MDI+", "HDI-"),
    breaks  = c("MDI", "HDI"),
    values = c("MDI"="#33ccff", "HDI"="#ff9933", "black"="#000000")
  ) + 
  theme( legend.key.width = unit(2,"cm")) 
mp1
```

![](drought2019_files/figure-html/hdiPlotFft-1.png)<!-- -->


## Plot Historical Humidity Index (HHI)


```r
library("RColorBrewer")
mp2 <- ggplot() +
  theme_classic(base_size=80) +
  #theme_classic() +  
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-4,4)) +
  labs(x="Year", y="HDI       HWI", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=hiFull$hwi5, x=hiFull$ts, color="a:HWI05"), size=1) +
  geom_line(aes(y=hiFull$hwi4, x=hiFull$ts, color="b:HWI04"), size=1.5) +
  geom_line(aes(y=hiFull$hwi3, x=hiFull$ts, color="c:HWI03"), size=2) +
  geom_line(aes(y=hiFull$hwi2, x=hiFull$ts, color="d:HWI02"), size=2.5) +
  geom_line(aes(y=hiFull$hwi1, x=hiFull$ts, color="e:HWI01"), size=3) +
  geom_line(aes(y=hiFull$hdi5, x=hiFull$ts, color="j:HDI05"), size=1) +  
  geom_line(aes(y=hiFull$hdi4, x=hiFull$ts, color="i:HDI04"), size=1.5) + 
  geom_line(aes(y=hiFull$hdi3, x=hiFull$ts, color="h:HDI03"), size=2) +  
  geom_line(aes(y=hiFull$hdi2, x=hiFull$ts, color="g:HDI02"), size=2.5) +  
  geom_line(aes(y=hiFull$hdi1, x=hiFull$ts, color="f:HDI01"), size=3) +
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
  theme( legend.key.width = unit(1,"cm")) 
mp2
```

![](drought2019_files/figure-html/hhiPlot-1.png)<!-- -->

## Calculate Lowpass FFT of HHI


```r
pt1 <- hhi
pt1 <- pt1[order(pt1$ts),]
pic <- pt1$hhi
len <- length(pic)
## mirror pic
pic <- append(pic, pic)
for(i in 1:len) {
  pic[i+len] <- pic[1+len-i]
}
frq <- fft(pic, inverse = FALSE)
frq0 <- frq
frq1 <- frq
frq5 <- frq 
filterYears = 1.0   #filter 1y
start = round(len/(12*filterYears))
stop  = round(2*len-start)
frq1[start:stop] <- 0.0 
filterYears = 5.0   #filter 5y
start = round(len/(12*filterYears))
stop  = round(2*len-start)
frq5[start:stop] <- 0.0 
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
pt1$prec1 <- pic1
pic5 <- Re(fft(frq5, inverse = TRUE)/length(frq5))
pic5 <- pic5[1:len]
pt1$prec5 <- pic5
```


## Plot Historical Humidity Index


```r
library("RColorBrewer")
library("ggplot2")
hhiColors = brewer.pal(n = 9, name = "Spectral")
mp <- ggplot(pt1, aes())
mp + geom_raster(aes(year,month, fill=hhi))+
  geom_raster(aes(year,-1, fill=1.5*prec5))+
  geom_raster(aes(year,-2, fill=0.75*prec1))+
  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(-2,-1,1,6,12), lab=c("1y","5y","1","6","12"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  #scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  scale_fill_gradientn(colors=hhiColors) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE)) +
  geom_hline(aes(yintercept = 6+0)) +
  geom_line(aes(y=6+1.5*pt1$prec5, x=pt1$ts, color="Filtered"), size=2.5) +
  scale_color_manual("Filtered", values=c("#000000"), labels=c("5y"))    
```

![](drought2019_files/figure-html/hdiPlot-1.png)<!-- -->


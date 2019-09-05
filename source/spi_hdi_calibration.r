library(zoo)
library("ggplot2")
library(gridExtra)
library(grid)
library(qdapTools)

setwd("C:/Code/climData/glaser2019")

spifull <- read.csv("https://raw.githubusercontent.com/climdata/dwdSPI/master/csv/spi_de.csv", sep=",", na = "NA")
spi <- subset(spifull, spifull$year>1880 & spifull$year<1996)

hi <- read.csv("csv/p_1000_2018.csv", sep=",", na = "NA")
hi <- subset(hi, hi$year>1499)

hi$hdi1 <- hi$precipitation
prev <- hi$hdi1
for (m in c(2,3,4,5,6,7,8,9,10,11,12)) {
  column <- paste("hdi", m, sep="")
  hdi <- rollapply(hi$precipitation, width=m, by=1, FUN=sum)
  hi$hdi <- prev
  hi$hdi[m:length(hi$hdi)] <- hdi
  prev <- hi$hdi
  names(hi)[names(hi) == 'hdi'] <- column
}

hic <- subset(hi, hi$year>1880 & hi$year<1996)

hdispi <- merge(hic,spi, by=c("year","month"))

plots <-  list()
slopes <- list()
par(mfrow=c(4,3))
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
  
  predx <- seq(-3, +3, len=50)
  pred <- predict(mx, newdata=list(x=predx), se.fit=TRUE)  
  
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(unname(coef(mx)[1]), digits = 3),
                        b = format(unname(coef(mx)[2]), digits = 5),
                        r2 = format(summary(mx)$r.squared, digits = 3)))
  eq <- as.character(as.expression(eq))
  p <- ggplot(data = df, aes(x = x, y = y)) +
    theme_classic(base_size=60) +
    scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-3.5,3.5)) +
    scale_y_continuous(breaks=yBreaks) +
    geom_hline(aes(yintercept=0)) +
    geom_vline(aes(xintercept=0)) +
    labs(x=toupper(spiCol), y=toupper(hdiCol), title="", subtitle="") +
    geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) +

#    geom_path(x=predx, y=pred$fit+2*pred$se.fit, color='cyan', lwd=5) +
#    geom_path(x=predx, y=pred$fit-2*pred$se.fit, color='cyan', lwd=5) +
#    geom_path(x=predx, y=pred$fit+0*pred$se.fit, color='#0000AA', lwd=5) +
        
    geom_point(color="#0000AA", alpha=0.3, size=5) +
    geom_text(x = 1.4, y = -3*slope, label = eq, parse = TRUE, size=15)
  plots[[length(plots) + 1]] <- p
  slopes[[length(slopes) + 1]] <- slope
  #ggsave(filename=paste("spi_hpi_", m, ".png", sep=""), device="png", width = 150, height = 150, unit = "mm", dpi = 600) 
}



margin = theme(plot.margin = unit(c(1,1,1,1), "cm"))
p <- grid.arrange(grobs = lapply(plots, "+", margin))

df <- list2df(slopes, col1="y")
df$x <- c(1:12)
mx <- lm(log(df$y) ~ log(df$x), df)

pred <- predict(mx, se.fit=TRUE)

eq <- substitute(italic(slope) == a %.% italic(months)^b*","~~italic(r)^2~"="~r2, 
                 list(a = format(exp(unname(coef(mx)[1])), digits = 4),
                      b = format(unname(coef(mx)[2]), digits = 4),
                      r2 = format(summary(mx)$r.squared, digits = 4)))
eq <- as.character(as.expression(eq))

p <- ggplot(data = df, aes(x = x, y = y)) +
  theme_classic(base_size=80) +
  labs(x="Months", y="Slope", title="", subtitle="") +
  geom_path(x=df$x, y=exp(pred$fit+2*pred$se.fit), color='#0088bb', lwd=5) +
  geom_path(x=df$x, y=exp(pred$fit-2*pred$se.fit), color='#0088bb', lwd=5) +
  geom_path(x=df$x, y=exp(pred$fit+0*pred$se.fit), color='#0000AA', lwd=5) +
  geom_point(color="#0000AA", size=25) + geom_text(x = 8, y = 2, label = eq, parse = TRUE, size=50)

p

## calculate SPI from HPI for 1500-xxxx

hspi <- hi[,c('year','month','time')]
for (m in c(1:12)) {
  hdiCol <- paste("hdi", m, sep="")
  spiCol <- paste("spi", m, sep="")
  spi <- hi[,hdiCol] * m^(-1/sqrt(3.0))
  hspi$spi <- round(spi, digits=6)
  names(hspi)[names(hspi) == 'spi'] <- spiCol
}

## Add rezent spi data 
spinew <- subset(spifull, spifull$time>max(hspi$time))
hspi <- rbind(hspi, spinew)


write.table(hspi, file = "csv/spi_1500_2019.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "escape", fileEncoding = "UTF-8")

mp1 <- ggplot(hspi, aes()) +
  theme_classic(base_size=80) +
  #coord_cartesian(ylim=c(0,300)) +
  labs(x="Year", y="SPI", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=hspi$spi3, x=hspi$time, color="SPI03"), size=2) +
  geom_line(aes(y=hspi$spi6, x=hspi$time, color="SPI06"), size=2) +
  geom_line(aes(y=hspi$spi12, x=hspi$time, color="SPI12"), size=2) +
  scale_color_manual(
    name = "", 
    labels = c("SPI3", "SPI6", "SPI12"),
    breaks  = c("SPI03", "SPI06", "SPI12"),
    values = c("SPI03"="#33ddff", "SPI06"="#0088bb","SPI12"="#003344")
  ) + 
  #guides(linetype = guide_legend(override.aes = list(size = 6))) +
  theme( legend.key.width = unit(2,"cm")) 

mp1



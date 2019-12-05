require("ggplot2")

spi_all <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/csv/spi_1500_2xxx.csv", sep=",")

spi.jan <- subset(spi_all, spi_all$month == 1)
spi.apr <- subset(spi_all, spi_all$month == 4)   # w
spi.jul <- subset(spi_all, spi_all$month == 7)
spi.oct <- subset(spi_all, spi_all$month == 10)  # s


mp <- ggplot()
mp + geom_line(aes(y=spi.apr$spi6, x=spi.apr$year), color="blue") + 
  geom_line(aes(y=spi.oct$spi6, x=spi.oct$year), color="red")


filterYears = 5.0   #filter 5y
#filterYears = 50
#FFT for HDI
pic <- spi.apr$spi6
len <- length(pic)
## mirror pic
pic <- append(pic, pic)
for(i in 1:len) {
  pic[i+len] <- pic[1+len-i]
}
frq <- fft(pic, inverse = FALSE)
frq1 <- frq
start = round(len/(1*filterYears)) ## yearly
stop  = round(2*len-start)
frq1[start:stop] <- 0.0 
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
spi.apr$spi6.5y <- pic1

pic <- spi.oct$spi6
len <- length(pic)
## mirror pic
pic <- append(pic, pic)
for(i in 1:len) {
  pic[i+len] <- pic[1+len-i]
}
frq <- fft(pic, inverse = FALSE)
frq1 <- frq
start = round(len/(1*filterYears)) ## yearly
stop  = round(2*len-start)
frq1[start:stop] <- 0.0 
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
spi.oct$spi6.5y <- pic1

mp <- ggplot()
mp + geom_line(aes(y=spi.apr$spi6.5y, x=spi.apr$year), color="blue") + 
  geom_line(aes(y=spi.oct$spi6.5y, x=spi.oct$year), color="red")

#22, 58, 37
apr.ts <- ts(spi.apr$spi6, frequency=58, start=c(1500,1))
apr.comp <- decompose(apr.ts)
plot(apr.comp)

oct.ts <- ts(spi.oct$spi6, frequency=58, start=c(1500,1))
oct.comp <- decompose(oct.ts)
plot(oct.comp)


## low-pass
apr.ts <- ts(spi.apr$spi6.5y, frequency=58, start=c(1500,1))
apr.comp <- decompose(apr.ts)
plot(apr.comp)

oct.ts <- ts(spi.oct$spi6.5y, frequency=58, start=c(1500,1))
oct.comp <- decompose(oct.ts)
plot(oct.comp)
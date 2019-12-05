require("ggplot2")

spi_all <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/csv/spi_1500_2xxx.csv", sep=",")

spi.jan <- subset(spi_all, spi_all$month == 1)
spi.apr <- subset(spi_all, spi_all$month == 4)   # w
spi.jul <- subset(spi_all, spi_all$month == 7)
spi.oct <- subset(spi_all, spi_all$month == 10)  # s


mp <- ggplot()
mp + geom_line(aes(y=spi.oct$spi12, x=spi.oct$year), color="black")


filterYears = 5.0   #filter 5y
filterYears = 25
#FFT for HDI

pic <- spi.oct$spi12
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
spi.oct$spi12.5y <- pic1

mp <- ggplot()
mp + geom_line(aes(y=spi.oct$spi12.5y, x=spi.oct$year), color="black")


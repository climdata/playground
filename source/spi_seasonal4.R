require("ggplot2")

spi_all <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/csv/spi_1500_2xxx.csv", sep=",")

spi.feb <- subset(spi_all, spi_all$month == 2)   # w
spi.may <- subset(spi_all, spi_all$month == 5)   # f
spi.aug <- subset(spi_all, spi_all$month == 8)   # s
spi.nov <- subset(spi_all, spi_all$month == 11)  # h


mp <- ggplot()
mp + geom_line(aes(y=spi.feb$spi3, x=spi.feb$year), color="blue") +
  geom_line(aes(y=spi.may$spi3, x=spi.may$year), color="green") +
  geom_line(aes(y=spi.aug$spi3, x=spi.aug$year), color="red") +
  geom_line(aes(y=spi.nov$spi3, x=spi.nov$year), color="brown")




filterYears = 5.0   #filter 5y
filterYears = 100
#FFT for HDI
pic <- spi.may$spi3
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
spi.may$spi3.5y <- pic1

pic <- spi.nov$spi3
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
spi.nov$spi3.5y <- pic1

pic <- spi.feb$spi3
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
spi.feb$spi3.5y <- pic1

pic <- spi.aug$spi3
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
spi.aug$spi3.5y <- pic1

mp <- ggplot()
mp + geom_line(aes(y=spi.feb$spi3.5y, x=spi.feb$year), color="blue") +
  geom_line(aes(y=spi.may$spi3.5y, x=spi.may$year), color="green") +
  geom_line(aes(y=spi.aug$spi3.5y, x=spi.aug$year), color="red") +
  geom_line(aes(y=spi.nov$spi3.5y, x=spi.nov$year), color="brown")

#22, 58, 37
feb.ts <- ts(spi.feb$spi3, frequency=58, start=c(1500,1))
feb.comp <- decompose(feb.ts)
plot(feb.comp)

may.ts <- ts(spi.may$spi3, frequency=58, start=c(1500,1))
may.comp <- decompose(may.ts)
plot(may.comp)

aug.ts <- ts(spi.aug$spi3, frequency=58, start=c(1500,1))
aug.comp <- decompose(aug.ts)
plot(aug.comp)

nov.ts <- ts(spi.nov$spi3, frequency=58, start=c(1500,1))
nov.comp <- decompose(nov.ts)
plot(nov.comp)
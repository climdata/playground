library("ggplot2")

t0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/ti_1500_2xxx_monthly.csv", sep=",", na = "NA")
t1 <- t0[,c("year","month","ti")]
p0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/pi_1500_2xxx_monthly.csv", sep=",", na = "NA")
prec0 <- read.csv("https://raw.githubusercontent.com/climdata/dwdTemperature/master/csv/monthly_temperature_de.csv", sep=",", na = "NA")
prec1 <- prec0[, c("year","month","Deutschland")]

tpsFull <- merge(t1,p0, by=c("year","month"))
tpsFull$t1 <- sin((tpsFull$month-1)*pi/6)
tpsFull$t2 <- cos((tpsFull$month-1)*pi/6)
tpsFull$t3 <- sin((tpsFull$month-1)*pi/3)
tpsFull$t4 <- cos((tpsFull$month-1)*pi/3)
tpsFull$t5 <- sin((tpsFull$month-1)*pi/2)
tpsFull$t6 <- cos((tpsFull$month-1)*pi/2)

tpsCal <- merge(tpsFull,prec1, by=c("year","month"))

tpsCal <- tpsCal[order(tpsCal$ts),]
tpsFull <- tpsFull[order(tpsFull$ts),]

mx <- lm(Deutschland ~ (pi+ti)*(t1+t2+t3+t4+t5+t6), tpsCal)
summary(mx)
prCal <- predict(mx, newdata=tpsCal, se.fit=TRUE) 
tpsCal$temp <- prCal$fit
tpsCal$temp.se <- prCal$se


p <- ggplot(data = tpsCal, aes(x = Deutschland, y = temp)) +
  geom_point(aes(y = temp), color="#00FF00", alpha=0.5, size=2)
p




prFull <- predict(mx, newdata=tpsFull, se.fit=TRUE) 
tpsFull$temperature <- prFull$fit
tpsFull$temperature.se <- prFull$se


#FFT 
pic <- tpsFull$temperature
len <- length(pic)
## mirror pic
pic <- append(pic, pic)
for(i in 1:len) {
  pic[i+len] <- pic[1+len-i]
}
frq <- fft(pic, inverse = FALSE)
frq1 <- frq
filterYears = 5.0   #filter 5y
start = round(len/(12*filterYears))
stop  = round(2*len-start)
frq1[start:stop] <- 0.0 
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
tpsFull$temp5 <- pic1

p <- ggplot()
p + geom_line(aes(y=tpsFull$temp5, x=tpsFull$ts), color="blue")


p <- ggplot(data = tpsFull, aes(x = ts, y = temp5)) +
  geom_line(aes(y = temp5), color="#FF0000", size=2)
p

color.temperature <- c("#0000FF", "#00CCCC", "#FFFFFF", "#EEAA33", "#FF5555")

mp <- ggplot(tpsFull, aes(year, month))
mp + geom_raster(aes(fill=temperature))+
  scale_y_continuous(breaks=c(1,6,12))+
  geom_line(aes(y=tpsFull$temp5, x=tpsFull$ts), color="blue") +
  theme_classic(base_size=80) +
  theme(panel.background = element_rect(fill = '#EEEEEE', colour = 'white'), legend.position="right", text=element_text(size=14))+
  scale_fill_gradientn(colours=color.temperature)
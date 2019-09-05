#install.packages("ggplot2")


require("ggplot2")
library(zoo)
par(mfrow=c(1,1))

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


setwd("C:/Code/climData/glaser2019")

pt1 <- read.csv("csv/p_1000_2018.csv", sep=",", na = "NA")
pt1$m <- NULL
spi <- read.csv("https://raw.githubusercontent.com/climdata/dwdSPI/master/csv/spi_de.csv", sep=",", na = "NA")
spi <- subset(spi, spi$time>(0.05+max(pt1$time)))
names(spi)[names(spi) == 'spi1'] <- 'precipitation'
spi <- spi[,c("year", "month","time","precipitation")]
pt1 <- rbind(pt1, spi)

hhi <- read.csv("csv/hhi_1500_2019.csv", sep=",", na = "NA")
pt1 <- subset(pt1, pt1$year>1499)

 

hist(10*hhi$hhi)

filterMonth <- 12*5
pt1 <- merge(pt1,hhi[,c("year", "month","hhi")], by=c("year", "month"))
hhim <- rollapply(pt1$hhi, width=filterMonth, by=1, FUN=sum)
pt1$hhim <- pt1$hhi
pt1$hhim[filterMonth:length(pt1$hhim)] <- hhim/filterMonth

pt1 <- pt1[order(pt1$time),]

pic <- pt1$precipitation
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

#pt1$prec1 <- pt8$precipitation
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
pt1$prec1 <- pic1
pic5 <- Re(fft(frq5, inverse = TRUE)/length(frq5))
#pic5 <- rollapply(pic5, width=24, by=1, FUN=mean)
pic5 <- pic5[1:len]
pt1$prec5 <- pic5

amp <- cbind(0:(length(frq)-1), Mod(frq))
plot(amp, t="h", lwd=2, main="", 
     xlab="Frequency", ylab="Ampl", 
     xlim=c(0,length(amp)/2), ylim=c(0,max(Mod(frq))))

plot(amp, t="h", lwd=2, main="", 
     xlab="Frequency", ylab="Ampl", 
     xlim=c(0,start), ylim=c(0,max(Mod(frq))))

#df <- data.frame(freq = 0:99, amp = Mod(frq)[1:100])
#df$freq <- length(frq)/(12*df$freq)
#ggplot(aes(), data=df) +
#  geom_line(aes(y=df$amp, x=df$freq)) +
#  scale_x_log10() + scale_x_reverse()  

mp <- ggplot(pt1, aes())
mp + geom_raster(aes(year,month, fill=precipitation))+
  geom_raster(aes(year,-1, fill=4*prec5))+
  geom_raster(aes(year,-2, fill=2*prec1))+
  #theme_classic(base_size=80) +
  theme_classic() +
  scale_y_continuous(breaks=c(-2,-1,1,6,12), lab=c("1y","5y","1","6","12"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE)) +
  geom_hline(aes(yintercept = 6+0)) +
  geom_line(aes(y=6+4.0*pt1$prec5, x=pt1$time, color="Filtered")) +
  scale_color_manual("Filtered", values=c("black"), labels=c("5y"))    
  #geom_line(aes(y=10+0.5*pt1$hhim, x=pt1$time))   


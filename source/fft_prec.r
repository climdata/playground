#install.packages("ggplot2")
#install.packages("RColorBrewer")
#install.packages("tsbox")

library("RColorBrewer")
require("ggplot2")
require("tsbox")

par(mfrow=c(1,1))

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


setwd("C:/Code/climData/glaser2019")

pt1 <- data.frame()
pt0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/p_1500_2xxx.csv", sep=",", na = "NA")
monthNames = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for(i in 1:length(monthNames)) {
  ptnew <- data.frame(year = pt0$year, month = i)
  prec <- pt0[,monthNames[i]]
  ptnew$precipitation <- prec
  pt1 <- rbind(pt1, ptnew)
}

pt1$time = pt1$year + (pt1$month+0.5)/12.0
pt1 <- pt1[order(pt1$time),]

a <- ts_pc(pt1)



## Calibrate
##pt1$hhi <- pt1$hhi + 5*sin(2*3.14159*(pt1$time-1500)*10/500) ##50y

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
pic5 <- pic5[1:len]
pt1$prec5 <- pic5

amp <- cbind(0:(length(frq)-1), Mod(frq))
plot(amp, t="h", lwd=2, main="", 
     xlab="Frequency", ylab="Ampl", 
     xlim=c(0,length(amp)/2), ylim=c(0,max(Mod(frq))))

plot(amp, t="h", lwd=2, main="", 
     xlab="Frequency", ylab="Ampl", 
     xlim=c(0,start), ylim=c(0,max(Mod(frq))))

## year = length(frq)/(12*frq)

breakYears=c(10,11,12,13,14,16,18,22,29,37,58,100,500)
breaksFreq=length(frq)/(12*breakYears)
labelsYears=lapply(breakYears, (function(year) substitute(frac(1, y), list(y=year)) ))
labelsYears=lapply(breakYears, (function(year) paste("1", year, sep="/") ))

df <- data.frame(freq = 0:(start), amp = Mod(frq)[1:(start+1)])
df$labels <- length(frq)/(12*df$freq)
#subset(df, df$amp>1000)
ggplot(aes(), data=df) +
  theme_classic(base_size=80) +
  geom_segment(aes(y=df$amp, x=df$freq, xend = df$freq, yend = 0.0*df$amp), size=4) +
  xlab("frequency [1/y]") + ylab("amplitude") +
  #scale_x_continuous(breaks=c(0,10,20,30,40,50,60,70,80,90,100))
  scale_x_continuous(breaks=breaksFreq, lab=labelsYears)

precColors = c("#AA6010", "#FCF0C2","#23AB30")

#display.brewer.all()




mp <- ggplot(pt1, aes())
mp + geom_raster(aes(year,month, fill=precipitation))+
  geom_raster(aes(year,-1, fill=3*prec5))+
  geom_raster(aes(year,-2, fill=1*prec1))+
  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(-2,-1,1,6,12), lab=c("1y","5y","1","6","12"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  #scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  scale_fill_gradientn(colors=precColors) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="PI", reverse = TRUE)) +
  geom_hline(aes(yintercept = 6+0)) +
  geom_line(aes(y=6+3*pt1$prec5, x=pt1$time, color="Filtered"), size=2) +
  scale_color_manual("Filtered", values=c("#000000"), labels=c("5y"))    
  #geom_line(aes(y=10+0.5*pt1$hhim, x=pt1$time))   


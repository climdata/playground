#install.packages("ggplot2")


require("ggplot2")
par(mfrow=c(1,1))

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


setwd("C:/Code/climData/glaser2019")

pt1 <- read.csv("csv/p_1000_2018.csv", sep=",", na = "NA")
pt1$m <- NULL
pt1 <- subset(pt1, pt$year>1499)


pt2 <- subset(pt1, !is.na(pt1$precipitation))

mp <- ggplot(pt2, aes(year, month))
mp + geom_raster(aes(fill=precipitation))+
  #theme_classic(base_size=80) +
  theme_classic() +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="PI", reverse = TRUE))  


frq <- fft(pt1$precipitation, inverse = FALSE)
frq2 <- frq

mA <- mean(Mod(frq)) 
sA   <- sd(Mod(frq))

#frq <- replace(frq, Mod(frq)<mA+2*sA, 0.0)
frq[2000:4000] <- 0.0
frq[490:5510] <- 0.0

frq3 <- frq

mA <- mean(Mod(frq[0:100])) 
sA   <- sd(Mod(frq[0:100]))

frq <- replace(frq, Mod(frq)<(mA+sA), 0.0)

for(i in 1:length(frq)) {
  frq[i] <- frq[i]*max(Mod(frq[i])-mA-sA, 0.0)/Mod(frq[i])
}


frq[100:5900] <- 0.0

frq[492:504] <- frq2[492:504]
frq[5490:5525] <- frq2[5490:5525]
frq[992:1004] <- frq2[992:1004]
frq[4990:5025] <- frq2[4990:5025]


amp <- cbind(0:(length(frq)-1), Mod(frq))
plot(amp, t="h", lwd=2, main="", 
     xlab="Frequency", ylab="Ampl", 
     xlim=c(0,length(amp)/2), ylim=c(0,max(Mod(frq))))

plot(amp, t="h", lwd=2, main="", 
     xlab="Frequency", ylab="Ampl", 
     xlim=c(0,100), ylim=c(0,max(Mod(frq))))

pt8 <- pt1
pt8$precipitation <- Re(fft(frq3, inverse = TRUE)/length(frq3))


pt9 <- pt1
pt9$precipitation <- Re(fft(frq, inverse = TRUE)/length(frq))


mp <- ggplot(pt8, aes(year, month))
mp + geom_raster(aes(fill=precipitation))+
  #theme_classic(base_size=80) +
  theme_classic() +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  theme( legend.key.width = unit(2,"cm")) +
  geom_line(aes(y=6+3*pt9$precipitation, x=pt9$time)) +
  guides(fill=guide_legend(title="PI", reverse = TRUE))  


mp <- ggplot()
mp + geom_line(aes(y=pt9$precipitation, x=pt9$time))

mp <- ggplot()
mp + geom_line(aes(y=pt2$precipitation, x=pt2$time))



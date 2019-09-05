#install.packages("ggplot2")
#install.packages("RColorBrewer")
#install.packages("zoo")

library("RColorBrewer")
require("ggplot2")
library(zoo)

par(mfrow=c(1,1))

setwd("C:/Code/climData/glaser2010")


t0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/t_1500_2006.csv", sep=",", na = "NA")
p0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/p_1500_1995.csv", sep=",", na = "NA")

spifull <- read.csv("https://raw.githubusercontent.com/climdata/dwdSPI/master/csv/spi_de.csv", sep=",", na = "NA")
baurfull <- read.csv("https://raw.githubusercontent.com/climdata/baur/master/csv/baur_yearly.csv", sep=",")

avg <- rollapply(baurfull, width=30, by=1, FUN=mean)
std <- rollapply(baurfull, width=30, by=1, FUN=sd)

last <-tail(baurfull, n=-30)

norm <- (last-avg)/std
norm$year <- last$year
norm <-tail(norm, n=-30)
norm$win <- NULL
norm$spr <- NULL
norm$sum <- NULL
norm$aut <- NULL
norm$avg <- NULL

normnew <- subset(norm, norm$year>max(t0$year))
t0 <- rbind(t0, normnew)


t1 <- data.frame()
p1 <- data.frame()
monthNames = c("jan","feb","mar","apr","mai","jun","jul","aug","sep","oct","nov","dec")
for(i in 1:length(monthNames)) {
  tnew <- data.frame(year = t0$year, month = i)
  tnew$temp0 <-  t0[,monthNames[i]]
  t1 <- rbind(t1, tnew)
  pnew <- data.frame(year = p0$year, month = i)
  pnew$prec0 <- p0[,monthNames[i]]
  p1 <- rbind(p1, pnew)
}

t1$time = t1$year + (t1$month+0.5)/12.0
t1 <- t1[order(t1$time),]
p1$time = p1$year + (p1$month+0.5)/12.0

spinew <- subset(spifull, spifull$time>max(p1$time))
spinew <- spinew[, c("year","month","time","spi1")]
names(spinew)[names(spinew) == 'spi1'] <- 'prec0'
p1 <- rbind(p1, spinew)
p1 <- p1[order(p1$time),]

frqFilter <- function(data, years=1.0) {
  pic <- data
  len <- length(pic)
  ## mirror pic
  pic <- append(pic, pic)
  for(i in 1:len) {
    pic[i+len] <- pic[1+len-i]
  }
  
  frq <- fft(pic, inverse = FALSE)
  frq0 <- frq
  frq1 <- frq

  start = round(len/(12*years))
  stop  = round(2*len-start)
  frq1[start:stop] <- 0.0 
  
  pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
  pic1 <- pic1[1:len]
  return(pic1)  
}

t1$temp1 <- frqFilter(t1$temp0, years=1.0)
t1$temp5 <- frqFilter(t1$temp0, years=5.0)
p1$prec1 <- frqFilter(p1$prec0, years=1.0)
p1$prec5 <- frqFilter(p1$prec0, years=5.0)

p1$time <- NULL
b1 <- merge(t1,p1, by=c("year","month"))

# xmax, xmin, xmid, ymax, ymin, ymid, x, y
# color max, mid, min
mixColor <- function(xList, yList,
                     #xmin=-1, xmid=0, xmax=1,
                     xmin=min(xList, na.rm = TRUE), xmid=mean(xList, na.rm = TRUE), xmax=max(xList, na.rm = TRUE),
                     #ymin=-1, ymid=0, ymax=1,
                     ymin=min(yList, na.rm = TRUE), ymid=mean(yList, na.rm = TRUE), ymax=max(yList, na.rm = TRUE),
                     cxmin="#FF0000", cxmid="#000000", cxmax="#0000FF",
                     cymin="#FFFF00", cymid="#000000", cymax="#00FF00"
){
  ## x and y must be same length
  colList <- c()

  

  for (i in 1:length(xList)) {
    
    
    cul = rgb(colorRamp(c(cxmin, cymax))( 0.5 )/255) 
    cum = rgb(colorRamp(c(cxmid, cymax))( 0.5 )/255) 
    cur = rgb(colorRamp(c(cxmax, cymax))( 0.5 )/255) 
    cml = rgb(colorRamp(c(cxmin, cymid))( 0.5 )/255) 
    cmm = rgb(colorRamp(c(cxmid, cymid))( 0.5 )/255) 
    cmr = rgb(colorRamp(c(cxmax, cymid))( 0.5 )/255) 
    cll = rgb(colorRamp(c(cxmin, cymin))( 0.5 )/255) 
    clm = rgb(colorRamp(c(cxmid, cymin))( 0.5 )/255) 
    clr = rgb(colorRamp(c(cxmax, cymin))( 0.5 )/255)       
    
    x=xList[i]
    y=yList[i] 
  
    if(is.na(x)) {
      x<-y
      cul = rgb(colorRamp(c(cymin, cymax))( 0.5 )/255) 
      cum = rgb(colorRamp(c(cymid, cymax))( 0.5 )/255) 
      cur = rgb(colorRamp(c(cymax, cymax))( 0.5 )/255) 
      cml = rgb(colorRamp(c(cymin, cymid))( 0.5 )/255) 
      cmm = rgb(colorRamp(c(cymid, cymid))( 0.5 )/255) 
      cmr = rgb(colorRamp(c(cymax, cymid))( 0.5 )/255) 
      cll = rgb(colorRamp(c(cymin, cymin))( 0.5 )/255) 
      clm = rgb(colorRamp(c(cymid, cymin))( 0.5 )/255) 
      clr = rgb(colorRamp(c(cymax, cymin))( 0.5 )/255)
      if(is.na(xmin)) { xmin <- ymin }
      if(is.na(xmid)) { xmid <- ymid }
      if(is.na(xmax)) { xmax <- ymax }
    } 
    if(is.na(y)) {
      y<-x 
      cul = rgb(colorRamp(c(cxmin, cxmax))( 0.5 )/255) 
      cum = rgb(colorRamp(c(cxmid, cxmax))( 0.5 )/255) 
      cur = rgb(colorRamp(c(cxmax, cxmax))( 0.5 )/255) 
      cml = rgb(colorRamp(c(cxmin, cxmid))( 0.5 )/255) 
      cmm = rgb(colorRamp(c(cxmid, cxmid))( 0.5 )/255) 
      cmr = rgb(colorRamp(c(cxmax, cxmid))( 0.5 )/255) 
      cll = rgb(colorRamp(c(cxmin, cxmin))( 0.5 )/255) 
      clm = rgb(colorRamp(c(cxmid, cxmin))( 0.5 )/255) 
      clr = rgb(colorRamp(c(cxmax, cxmin))( 0.5 )/255) 
      if(is.na(ymin)) { ymin <- xmin }
      if(is.na(ymid)) { ymid <- xmid }
      if(is.na(ymax)) { ymax <- xmax }
    } 
    
  if (x>xmid) {
    xn = (x-xmid)/(xmax-xmid)
    if (y>ymid) {
      yn = (y-ymid)/(ymax-ymid)
      c1 = rgb(colorRamp(c(cmm, cmr))( xn )/255)       
      c2 = rgb(colorRamp(c(cum, cur))( xn )/255)
    } else {
      yn = (y-ymid)/(ymin-ymid)
      c1 = rgb(colorRamp(c(cmm, cmr))( xn )/255)
      c2 = rgb(colorRamp(c(clm, clr))( xn )/255)      
    }
  } else {
    xn = (x-xmid)/(xmin-xmid)
    if (y>ymid) {
      yn = (y-ymid)/(ymax-ymid)
      c1 = rgb(colorRamp(c(cmm, cml))( xn )/255)       
      c2 = rgb(colorRamp(c(cum, cul))( xn )/255)
    } else {
      yn = (y-ymid)/(ymin-ymid)
      c1 = rgb(colorRamp(c(cmm, cml))( xn )/255)
      c2 = rgb(colorRamp(c(clm, cll))( xn )/255)       
    }    
  }
  c3 = rgb(colorRamp(c(c1, c2))( yn )/255)
  ## add c3 to result list
  colList <- c(colList, c3) 
  
  }
  return(colList)
}



mixTColor <- function(t,p) {
  c <- mixColor(t,t, 
                cxmin="#0505b3", cxmid="#F7F7F7", cxmax="#c90b04",
                cymin="#0505b3", cymid="#F7F7F7", cymax="#c90b04"                
  )
}
mixPColor <- function(t,p) {
  c <- mixColor(p,p, 
                cxmin="#AA6010", cxmid="#FCF0C2", cxmax="#23AB30",
                cymin="#AA6010", cymid="#FCF0C2", cymax="#23AB30"                
  )
}

mixTPColor <- function(t,p) {
  c <- mixColor(t,p, 
                cxmin="#0505b3", cxmid="#F7F7F7", cxmax="#c90b04",  
                cymin="#AA6010", cymid="#FCF0C2", cymax="#23AB30"                
  )
  return(c)
}



mp <- ggplot(b1, aes())
mp + geom_raster(aes(year,month), fill=mixTPColor(b1$temp0, b1$prec0))+
  #geom_raster(aes(year,-1, fill=3*temp5))+
  #geom_raster(aes(year,-2, fill=1*temp1))+
  geom_raster(aes(year, 15), fill=mixTColor(3*b1$temp5, 3*b1$temp5))+
  geom_raster(aes(year, 14), fill=mixPColor(3*b1$prec5, 3*b1$prec5))+  
  geom_raster(aes(year,-1), fill=mixTPColor(3*b1$temp5, 3*b1$prec5))+
  geom_raster(aes(year,-2), fill=mixTPColor(1*b1$temp1, 1*b1$prec1))+
  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(-2,-1,1,6,12,14,15), lab=c("1y","5y","1","6","12","P","T"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="TI/PI", reverse = TRUE)) +
  geom_hline(aes(yintercept = 9+0)) +
  geom_line(aes(y=9+2*b1$temp5, x=b1$time, color="Filtered"), size=2) +
  geom_hline(aes(yintercept = 3+0)) +
  geom_line(aes(y=3+2*b1$prec5, x=b1$time, color="Filtered"), size=2) +
  scale_color_manual("Filtered", values=c("#000000"), labels=c("5y"))    
  #geom_line(aes(y=10+0.5*pt1$hhim, x=pt1$time))   



## Legende
dl = data.frame()
for (i in 1:7) {
  for (j in 1:7) {
     nl = data.frame(i=i,j=j, t=i-4, p=j-4)
     dl <- rbind(dl, nl)
  }
}
for (i in 1:7) {
  nl = data.frame(i=i,j=9, t=i-4, p=NA)
  dl <- rbind(dl, nl)
  }
for (j in 1:7) {
  nl = data.frame(i=-1,j=j, t=NA, p=j-4)
  dl <- rbind(dl, nl)
}


mp <- ggplot(dl ,aes()) +
   theme_classic(base_size=80) +
   geom_raster(aes(i, j), fill=mixTPColor( dl$t, dl$p )) +
   scale_y_continuous(breaks=c(1,2,3,4,5,6,7,9), lab=c("-3","-2","-1","0","+1","+2","+3", "T")) +
   scale_x_continuous(breaks=c(-1,1,2,3,4,5,6,7), lab=c("P","-3","-2","-1","0","+1","+2","+3")) +
   xlab("Temperature") + ylab("Precipitation") 
mp


###

b1$temp2 <- frqFilter(b1$temp0, years=2.0)
b1$temp3 <- frqFilter(b1$temp0, years=3.0)
b1$temp4 <- frqFilter(b1$temp0, years=4.0)
b1$prec2 <- frqFilter(b1$prec0, years=2.0)
b1$prec3 <- frqFilter(b1$prec0, years=3.0)
b1$prec4 <- frqFilter(b1$prec0, years=4.0)


mp <- ggplot(b1, aes())
mp + 

  geom_raster(aes(year, 5), fill=mixTColor(1*b1$temp1, 1*b1$temp1))+  
  geom_raster(aes(year, 4), fill=mixTColor(1.5*b1$temp2, 1.5*b1$temp2))+   
  geom_raster(aes(year, 3), fill=mixTColor(2*b1$temp3, 2*b1$temp3))+  
  geom_raster(aes(year, 2), fill=mixTColor(2.5*b1$temp4, 2.5*b1$temp4))+  
  geom_raster(aes(year, 1), fill=mixTColor(3*b1$temp5, 3*b1$temp5))+
  geom_raster(aes(year, 0), fill=mixTPColor(3*b1$temp5, 3*b1$prec5))+
  geom_raster(aes(year,-1), fill=mixPColor(3*b1$prec5, 3*b1$prec5))+  
  geom_raster(aes(year,-2), fill=mixPColor(2.5*b1$prec4, 2.5*b1$prec4))+    
  geom_raster(aes(year,-3), fill=mixPColor(2*b1$prec3, 2*b1$prec3))+  
  geom_raster(aes(year,-4), fill=mixPColor(1.5*b1$prec2, 1.5*b1$prec2))+    
  geom_raster(aes(year,-5), fill=mixPColor(1*b1$prec1, 1*b1$prec1))+  

  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), lab=c("1y P","2y P","3y P","4y P","5y P","5y T&P","5y T", "4y T", "3y T", "2y T", "1y T"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="TI/PI", reverse = TRUE)) +
  geom_hline(aes(yintercept = 3+0)) +
  geom_line(aes(y=3+2*b1$temp5, x=b1$time, color="Filtered"), size=2) +
  geom_hline(aes(yintercept = -3+0)) +
  geom_line(aes(y=-3+2*b1$prec5, x=b1$time, color="Filtered"), size=2) +
  scale_color_manual("Filtered", values=c("#000000"), labels=c("5y"))    
#geom_line(aes(y=10+0.5*pt1$hhim, x=pt1$time))   



library("RColorBrewer")
require("ggplot2")
library(zoo)

par(mfrow=c(1,1))

setwd("C:/Code/climData/glaser2010")


t0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/t_1500_2xxx.csv", sep=",", na = "NA")
p0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/p_1500_2xxx.csv", sep=",", na = "NA")

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
monthNames = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
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
t1$temp2 <- frqFilter(t1$temp0, years=2.0)
t1$temp3 <- frqFilter(t1$temp0, years=3.0)
t1$temp4 <- frqFilter(t1$temp0, years=4.0)
t1$temp5 <- frqFilter(t1$temp0, years=5.0)

p1$prec1 <- frqFilter(p1$prec0, years=1.0)
p1$prec2 <- frqFilter(p1$prec0, years=2.0)
p1$prec3 <- frqFilter(p1$prec0, years=3.0)
p1$prec4 <- frqFilter(p1$prec0, years=4.0)
p1$prec5 <- frqFilter(p1$prec0, years=5.0)


p1$time <- NULL
b1 <- merge(t1,p1, by=c("year","month"))

b1 <- b1[order(b1$time),]

b1$vprec0 <- b1$prec0
b1$vprec0[1] <- 0
b1$vprec1 <- b1$prec1
b1$vprec1[1] <- 0
b1$vprec2 <- b1$prec2
b1$vprec2[1] <- 0
b1$vprec3 <- b1$prec3
b1$vprec3[1] <- 0
b1$vprec4 <- b1$prec4
b1$vprec4[1] <- 0
b1$vprec5 <- b1$prec5
b1$vprec5[1] <- 0
b1$vtemp0 <- b1$temp0
b1$vtemp0[1] <- 0
b1$vtemp1 <- b1$temp1
b1$vtemp1[1] <- 0
b1$vtemp2 <- b1$temp2
b1$vtemp2[1] <- 0
b1$vtemp3 <- b1$temp3
b1$vtemp3[1] <- 0
b1$vtemp4 <- b1$temp4
b1$vtemp4[1] <- 0
b1$vtemp5 <- b1$temp5
b1$vtemp5[1] <- 0
b1$vtp1 <- b1$temp1
b1$vtp1[1] <- 0
b1$vtp2 <- b1$temp2
b1$vtp2[1] <- 0
b1$vtp3 <- b1$temp3
b1$vtp3[1] <- 0
b1$vtp4 <- b1$temp4
b1$vtp4[1] <- 0
b1$vtp5 <- b1$temp5
b1$vtp5[1] <- 0

b1$aprec0 <- b1$vprec0
b1$aprec0[1] <- 0
b1$aprec1 <- b1$vprec1
b1$aprec1[1] <- 0
b1$aprec5 <- b1$vprec5
b1$aprec5[1] <- 0
b1$atemp0 <- b1$vtemp0
b1$atemp0[1] <- 0
b1$atemp1 <- b1$vtemp1
b1$atemp1[1] <- 0
b1$atemp5 <- b1$vtemp5
b1$atemp5[1] <- 0
b1$atp5 <- b1$vprec5
b1$atp5[1] <- 0

for (i in c(2:nrow(b1))) {
  b1$vprec0[i] = b1$prec0[i] - b1$prec0[i-1]  
  b1$vprec1[i] = b1$prec1[i] - b1$prec1[i-1]
  b1$vprec2[i] = b1$prec2[i] - b1$prec2[i-1]
  b1$vprec3[i] = b1$prec3[i] - b1$prec3[i-1]
  b1$vprec4[i] = b1$prec4[i] - b1$prec4[i-1]
  b1$vprec5[i] = b1$prec5[i] - b1$prec5[i-1]
  
  b1$vtemp0[i] = b1$temp0[i] - b1$temp0[i-1]
  b1$vtemp1[i] = b1$temp1[i] - b1$temp1[i-1]
  b1$vtemp2[i] = b1$temp2[i] - b1$temp2[i-1]
  b1$vtemp3[i] = b1$temp3[i] - b1$temp3[i-1]
  b1$vtemp4[i] = b1$temp4[i] - b1$temp4[i-1]
  b1$vtemp5[i] = b1$temp5[i] - b1$temp5[i-1]
  
  b1$aprec0[i] = b1$vprec0[i] - b1$vprec0[i-1]    
  b1$aprec1[i] = b1$vprec1[i] - b1$vprec1[i-1]  
  b1$aprec5[i] = b1$vprec5[i] - b1$vprec5[i-1]
  b1$atemp0[i] = b1$vtemp0[i] - b1$vtemp0[i-1]   
  b1$atemp1[i] = b1$vtemp1[i] - b1$vtemp1[i-1]   
  b1$atemp5[i] = b1$vtemp5[i] - b1$vtemp5[i-1]  
  
  b1$vtp1[i] = (b1$temp1[i] - b1$temp1[i-1])^2 + (b1$prec1[i] - b1$prec1[i-1])^2
  b1$vtp2[i] = (b1$temp2[i] - b1$temp2[i-1])^2 + (b1$prec2[i] - b1$prec2[i-1])^2
  b1$vtp3[i] = (b1$temp3[i] - b1$temp3[i-1])^2 + (b1$prec3[i] - b1$prec3[i-1])^2
  b1$vtp4[i] = (b1$temp4[i] - b1$temp4[i-1])^2 + (b1$prec4[i] - b1$prec4[i-1])^2
  b1$vtp5[i] = (b1$temp5[i] - b1$temp5[i-1])^2 + (b1$prec5[i] - b1$prec5[i-1])^2
 
  b1$atp5[i] = (b1$vtemp5[i] - b1$vtemp5[i-1])^2 + (b1$vprec5[i] - b1$vprec5[i-1])^2   
}


vul <-  read.csv("https://raw.githubusercontent.com/climdata/volcano/master/csv/monthly_volcano.csv", sep=",", na = "NA")
vul <- subset(vul, vul$year>1499)
vul <- vul[,c('year','month','time', 'vei')]
vul$month[is.na(vul$month)] <- 6 
for (y in c(1500:2018)) {
  for (m in c(1:12)) {
    check <- subset(vul, (vul$year==y & vul$month==m))
    if (nrow(check) == 0) {
      new <- data.frame(year = y, month = m, time=y+(m+0.5)/12, vei=0)
      vul <- rbind(vul, new)
    } 
  } 
}

vul$time <- NULL
b1 <- merge(b1,vul, by=c("year","month"))
b1 <- b1[order(b1$time),]

b1$vei5 <- frqFilter(b1$vei, years=5.0)
b1$vei1 <- frqFilter(b1$vei, years=1.0)

co2 <-  read.csv("https://raw.githubusercontent.com/climdata/co2/master/csv/monthly_co2.csv", sep=",", na = "NA")
for (y in c(1500:2018)) {
  for (m in c(1:12)) {
    check <- subset(co2, (co2$year==y & co2$month==m))
    if (nrow(check) == 0) {
      new <- data.frame(year = y, month = m, time=y+(m+0.5)/12, CO2=min(co2$CO2))
      co2 <- rbind(co2, new)
    } 
  } 
}

co2$time <- NULL
b1 <- merge(b1,co2, by=c("year","month"))
b1 <- b1[order(b1$time),]

b1$co25 <- frqFilter(b1$CO2, years=5.0)
b1$co21 <- frqFilter(b1$CO2, years=1.0)

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


mixSColor <- function(t,p) {
  c <- mixColor(t,t, 
                cxmin="#000000", cxmid="#a9ab33", cxmax="#f6fa0c",
                cymin="#000000", cymid="#a9ab33", cymax="#f6fa0c"                
  )
}

mixVColor <- function(t,p) {
  c <- mixColor(t,t, 
                cxmin="#555555", cxmid="#FF0000", cxmax="#FF3333",
                cymin="#555555", cymid="#FF0000", cymax="#ff3333"                
  )
}
mixSVColor <- function(t,p) {
  c <- mixColor(t,p,
                cxmin="#000000", cxmid="#a9ab33", cxmax="#f6fa0c",                 
                cymin="#555555", cymid="#FF0000", cymax="#FF3333"
                
  )
}

p <- ggplot(data = b1, aes(x = b1$prec5, y = b1$vprec5)) +
  geom_point(color=mixTColor(1*b1$temp5, 1*b1$temp5), alpha=0.5, size = (6+b1$month)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p
p <- ggplot(data = b1, aes(x = b1$prec1, y = b1$vprec1)) +
  geom_point(color=mixTColor(1*b1$temp1, 1*b1$temp1), alpha=0.5, size = (6+b1$month)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p


p <- ggplot(data = b1, aes(x = b1$temp5, y = b1$vtemp5)) +
  geom_point(color=mixPColor(1*b1$prec5, 1*b1$prec5), alpha=0.5, size = (6+b1$month)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p

p <- ggplot(data = b1, aes(x = b1$temp5, y = b1$prec5)) +
  geom_point(color=mixTPColor(1*b1$vtemp5, 1*b1$vprec5), alpha=0.5, size = (6+b1$month)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p

p <- ggplot(data = b1, aes(x = b1$vtemp5, y = b1$vprec5)) +
  geom_point(color=mixTPColor(1*b1$temp5, 1*b1$prec5), alpha=0.5, size = (6+b1$month)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p

sun <- read.csv("https://raw.githubusercontent.com/climdata/solar/master/csv/monthly_sunspots.csv", sep=",", na = "NA")

sun$time <- NULL
b2 <- merge(b1,sun, by=c("year","month"))
b2 <- b2[order(b2$time),]

b2$sunspots5 <- frqFilter(b2$sunspots, years=5.0)
b2$sunspots1 <- frqFilter(b2$sunspots, years=1.0)

p <- ggplot(data = b2, aes(x = temp5, y = prec5)) +
  geom_point(color=mixSColor(1*b2$sunspots5, 1*b2$sunspots5), alpha=0.5, size = 5) +
  theme_classic(base_size=60)
p


p <- ggplot(data = b2, aes(x = prec3, y = temp3)) +
  geom_point(color=mixSColor(1*b2$sunspots5, 1*b2$sunspots5), alpha=0.5, size =  (6+b2$month)) +
  theme_classic(base_size=60)
p

p <- ggplot(data = b2, aes(x = b2$temp5, y = b2$prec5)) +
  geom_point(color=mixSColor(1*b2$sunspots5, 1*b2$sunspots5), alpha=0.5, size = (4+b2$atp5*20000000)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p
  
p <- ggplot(data = b2, aes(x = b2$prec5, y = b2$vprec5)) +
  geom_point(color=mixTColor(1*b2$temp5, 1*b2$temp5), alpha=0.5, size = (4+b2$sunspots5/10)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p

p <- ggplot(data = b2, aes(x = b2$prec1, y = b2$vprec1)) +
  geom_point(color=mixSColor(1*b2$sunspots5, 1*b2$sunspots5), alpha=0.5, size = (6+b2$month)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p

p <- ggplot(data = b2, aes(x = prec5, y = vprec5)) +
  geom_point(color=mixTColor(1*b2$temp5, 1*b2$temp5), alpha=0.5, size =(4+b2$sunspots5/10)) +
  theme_classic(base_size=60)
p

p <- ggplot(data = b2, aes(x = b2$sunspots5, y = b2$atp5)) +
  geom_point(color=mixTPColor(1*b2$temp5, 1*b2$prec5), alpha=0.5, size = (4+b2$vtp5*25000)) +
  #scale_size(range = c(40, 150)) +
  theme_classic(base_size=60)
p

max(b2$prec5)


#install.packages("plot3D")
library("plot3D")

s1 <- b2[,c('year','month', 'sunspots5')]

for (i in 1:nrow(b1)) {
    y = b1$year[i]
    m = b1$month[i]
    check <- subset(s1, (s1$year==y & s1$month==m))
    if (nrow(check) == 0) {
      new <- data.frame(year = y, month = m, sunspots5=mean(b2$sunspots5))
      s1 <- rbind(s1, new)
    } 
}
#s1$time <- s1$year + (s1$month-0.5)/12
#s1 <- s1[order(s1$time),]

b1 <- merge(b1,s1, by=c("year","month"))
b1 <- b1[order(b1$time),]

scatter3D(x=b2$temp3, y=b2$prec3, z=b2$time, colvar=b2$sunspots1, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5), col = ramp.col(c("black", "yellow", "orange")))

scatter3D(x=b1$temp5, y=b1$prec5, z=b1$time, colvar=b1$sunspots5, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5), col = ramp.col(c("black", "yellow", "orange")) )

scatter3D(x=b1$vprec1, y=b1$vtemp1, z=b1$time, colvar=b1$prec1, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5), col = ramp.col(c("brown", "white", "green")))

scatter3D(x=b1$vprec5, y=b1$prec5, z=b1$temp5, colvar=b1$time, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5), col = ramp.col(c("yellow", "blue")))

scatter3D(x=b1$prec5, y=b1$vprec5, z=b1$time, colvar=b1$temp5, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5), col = ramp.col(c("blue", "grey", "red")))

library("plot3Drgl")
plotrgl()

cor(b2$prec5, b2$sunspots5)


b1$atemp5p <- b1$atemp5
b1$atemp5p[2:nrow(b1)] <- b1$atemp5[1:(nrow(b1)-1)]
b1$aprec5p <- b1$aprec5
b1$aprec5p[2:nrow(b1)] <- b1$aprec5[1:(nrow(b1)-1)]

b1$vtemp5p <- b1$vtemp5
b1$vtemp5p[2:nrow(b1)] <- b1$vtemp5[1:(nrow(b1)-1)]
b1$vprec5p <- b1$vprec5
b1$vprec5p[2:nrow(b1)] <- b1$vprec5[1:(nrow(b1)-1)]

b1$temp5p <- b1$temp5
b1$temp5p[2:nrow(b1)] <- b1$temp5[1:(nrow(b1)-1)]
b1$prec5p <- b1$prec1
b1$prec5p[2:nrow(b1)] <- b1$prec5[1:(nrow(b1)-1)]

library("psych")
pairs <- b2[,c('sunspots1', 'prec1', 'vprec1', 'aprec1', 'temp1', 'vtemp1', 'atemp1', 'vei1')]
pairs.panels(pairs, scale=TRUE, ci=TRUE, font.labels=45, cex.cor=20)

pairs <- b1[,c('prec5', 'vprec5', 'aprec5', 'temp5', 'vtemp5', 'atemp5','co25' ,'sunspots5','vei5')]
pairs.panels(pairs, scale=TRUE, ci=TRUE, font.labels=45, cex.cor=20)


## trick: only use 10% of previous value
b1$atemp5h <- b1$atemp5 - 0.1*b1$atemp5p
fm = glm(atemp5h ~ poly(vprec5,3) + poly(vtemp5,3) +  poly(prec5,3) + poly(temp5,3) +sunspots5 + vei5, data=b1, family=gaussian)
#fm = glm(atemp5h ~ poly(prec5,3) + poly(temp5,3) +sunspots5 + vei5, data=b2, family=gaussian)
summary(fm)
b1$atemp5new <- 0.1*b1$atemp5p +
 0-9.341e-03*b1$temp5 + 2.345e-03*b1$temp5^2-1.257e-03*b1$temp5^3+
 0+2.244e-03*b1$vtemp5^2-6.936e-04*b1$vtemp5^3+
 0+3.991e-03*b1$prec5+2.438e-03*b1$prec5^2-1.300e-03*b1$prec5^3+
 0-2.351e-03*b1$vprec5+
 0-9.395e-07*b1$sunspots5+2.699e-04*b1$vei5
# Rückstellkraft alone - use 50% in simulation
fm = glm(atemp5 ~ temp5, data=b1, family=gaussian)
summary(fm)


## trick: only use 10% of previous value
b1$aprec5h <- b1$aprec5 - 0.1*b1$aprec5p
fm = glm(aprec5h ~  poly(vprec5,3) + poly(vtemp5,3) + poly(prec5,3) + poly(temp5,3) + sunspots5 + vei5, data=b2, family=gaussian)
#fm = glm(aprec5h  ~  poly(prec5,3) + poly(temp5,3) + sunspots5, data=b2, family=gaussian)
summary(fm)  
b1$aprec5new <- 0.1*b1$aprec5p+
  0+3.991e-03*b1$prec5+2.438e-03*b1$prec5^2-1.300e-03*b1$prec5^3+
  0-2.351e-03*b1$vprec5+  
  0 -9.341e-03*b1$temp5+2.345e-03*b1$temp5^2-1.257e-03*b1$temp5^3+
  0+2.244e-03*b1$vtemp5^2-6.936e-04*b1$vtemp5^3+  
  0-9.395e-07*b1$sunspots5+2.699e-04*b1$vei5    
# Rückstellkraft alone - use 50% in simulation
fm = glm(aprec5 ~ prec5, data=b1, family=gaussian)
summary(fm)


# trick: this is the formular for integration
b1$vtemp5h <- b1$vtemp5 - b1$vtemp5p - b1$atemp5new

fm = glm(vtemp5h ~ poly(prec5,3) + poly(temp5,3) + sunspots5 + vei5 , data=b1, family=gaussian)
#fm = glm(vtemp5h ~ prec5 + temp5 , data=b1, family=gaussian)
summary(fm)
b1$vtemp5new <- b1$vtemp5p+b1$atemp5new-1.363e-04+
  0-4.321e-02*b1$prec5-1.059e-02*b1$prec5^2+
  0+1.757e-01*b1$temp5-1.415e-02*b1$temp5^2+4.039e-03*b1$temp5^3+
  0+1.463e-07*b1$sunspots5-5.321e-05*b1$vei5    


# trick: this is the formular for integration
b1$vprec5h <- b1$vprec5 - b1$vprec5p - b1$aprec5new

fm = glm(vprec5h ~ poly(prec5,3) + poly(temp5,3) + sunspots5 + vei5 , data=b1, family=gaussian)
#fm = glm(vprec5h ~ prec5 + temp5, data=b1, family=gaussian)
summary(fm)
b1$vprec5new <- b1$vprec5p+b1$aprec5new-1.484e-04+
  0 -5.571e-02*b1$prec5-1.302e-02*b1$prec5^2+
  0+1.870e-01*b1$temp5-1.750e-02*b1$temp5^2+6.109e-03*b1$temp5^3+
  0+6.908e-07*b1$sunspots5-1.395e-04*b1$vei5  

# trick: this is the formular for integration
b1$temp5h <- b1$temp5 - b1$temp5p - b1$vtemp5new

fm = glm(temp5h ~ sunspots5 + vei5, data=b1, family=gaussian)
summary(fm)
b1$temp5new <- b1$temp5p+b1$vtemp5new+4.415e-02+
               0-4.975e-05*b1$sunspots5-1.114e-01*b1$vei5

# trick: this is the formular for integration
b1$prec5h <- b1$prec5 - b1$prec5p - b1$vprec5new

fm = glm(prec5h ~ sunspots5 + vei5, data=b1, family=gaussian)
summary(fm)
b1$prec5new <- b1$prec5p+b1$vprec5new+4.508e-02+
  0-4.269e-05*b1$sunspots5-1.168e-01*b1$vei5

cor(b2$prec5, b2$prec5new)


set.seed(17)
rnorm(1,0,1e-8)

b1$atemp5new <- b1$atemp5 
b1$aprec5new <- b1$aprec5 
b1$vtemp5new <- b1$vtemp5 
b1$vprec5new <- b1$vprec5 
b1$temp5new <- b1$temp5 
b1$prec5new <- b1$prec5 
for (i in 2:nrow(b1)) {
  ## a
  deltaatemp = 0-9.341e-03*b1$temp5new[i-1] + 2.345e-03*b1$temp5new[i-1]^2-1.257e-03*b1$temp5new[i-1]^3 +
    0+2.244e-03*b1$vtemp5new[i-1]^2-6.936e-04*b1$vtemp5new[i-1]^3 +
    0-2.351e-03*b1$prec5new[i-1]+2.438e-03*b1$prec5new[i-1]^2-1.300e-03*b1$prec5new[i-1]^3 +
    0+3.991e-03*b1$vprec5new[i-1]+
    0-9.395e-07*b1$sunspots5[i-1]+ 
    0+2.699e-04*b1$vei5[i-1] + 
    0+rnorm(1,0,1e-9) +
    0-1.e-04*b1$temp5new[i-1]^5
  
  if(is.nan(deltaatemp)) {deltaatemp = 0.0}
  if(deltaatemp > 0.02) {deltaatemp = 0.02}
  if(deltaatemp < -0.02) {deltaatemp = -0.02}  
  b1$atemp5new[i] <- 0.1*b1$atemp5new[i-1] + deltaatemp
  b1$atemp5new[i] <- (b1$atemp5new[i] -4.014e-04*b1$temp5new[i-1])/2
  
    

  deltaaprec = 0+3.991e-03*b1$prec5new[i-1]+2.438e-03*b1$prec5new[i-1]^2-1.300e-03*b1$prec5new[i-1]^3 +  
    0-2.351e-03*b1$vprec5new[i-1]+
    0-9.341e-03*b1$temp5new[i-1]+2.345e-03*b1$temp5new[i-1]^2-1.257e-03*b1$temp5new[i-1]^3 +
    0+2.244e-03*b1$vtemp5new[i-1]^2-6.936e-04*b1$vtemp5new[i-1]^3 +
    0-9.395e-07*b1$sunspots5[i-1]+2.699e-04*b1$vei5[i-1] +
    0+rnorm(1,0,1e-9) +
    0-1.e-04*b1$prec5new[i-1]^5
  
  if(is.nan(deltaaprec)) {deltaaprec = 0.0}
  if(deltaaprec > 0.02) {deltaaprec = 0.02}
  if(deltaaprec < -0.02) {deltaaprec = -0.02}    
  
  b1$aprec5new[i] <- 0.1*b1$aprec5new[i-1]+deltaaprec
  b1$aprec5new[i] <- (b1$aprec5new[i] -6.442e-04*b1$prec5new[i-1])/2
  
  
 ## v
  
  deltavtemp = -1.363e-04+
    0-4.321e-02*b1$prec5new[i-1]-1.059e-02*b1$prec5new[i-1]^2 +  
    0+1.757e-01*b1$temp5new[i-1]-1.415e-02*b1$temp5new[i-1]^2+4.039e-03*b1$temp5new[i-1]^3 +
    0+1.463e-07*b1$sunspots5[i-1]-5.321e-05*b1$vei5[i-1]
  if(is.nan(deltavtemp)) {deltavtemp = 0.0}
  if(deltavtemp > 0.01) {deltavtemp = 0.01}
  if(deltavtemp < -0.01) {deltavtemp = -0.01}    
  b1$vtemp5new[i] <- b1$vtemp5new[i-1]+b1$atemp5new[i] + deltavtemp   

  deltavprec = -1.484e-04+
    0-5.571e-02*b1$prec5new[i-1]-1.302e-02*b1$prec5new[i-1]^2 +  
    0+1.870e-01*b1$temp5new[i-1]-1.750e-02*b1$temp5new[i-1]^2+6.109e-03*b1$temp5new[i-1]^3 +
    0+6.908e-07*b1$sunspots5[i-1]-1.395e-04*b1$vei5[i-1] 
  if(is.nan(deltavprec)) {deltavprec = 0.0}
  if(deltavprec > 0.01) {deltavprec = 0.01}
  if(deltavprec < -0.01) {deltavprec = -0.01}    
  b1$vprec5new[i] <- b1$vprec5new[i-1]+b1$aprec5new[i] + deltavprec  

  ## s
  b1$temp5new[i] <- b1$temp5new[i-1]+b1$vtemp5new[i]+4.415e-02+
    0+4.975e-05*b1$sunspots5[i-1]-1.114e-01*b1$vei5[i-1] 

  b1$prec5new[i] <- b1$prec5new[i-1]+b1$vprec5new[i]+4.508e-02+
    0-4.269e-05*b1$sunspots5[i-1]-1.168e-01*b1$vei5[i-1] 
}

p <- ggplot(data = b1, aes(x = b1$temp5new, y = b1$prec5new)) +
  geom_point(color="blue", alpha=0.5, size = 5) +
  #scale_size(range = c(40, 150)) +
  theme_classic() 
  #theme_classic(base_size=60)
p

scatter3D(x=b1$prec5new, y=b1$temp5new, z=b1$time, colvar=b1$solarspots5, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5), col = ramp.col(c("black", "yellow", "red")))

scatter3D(x=b1$prec5, y=b1$temp5, z=b1$time, colvar=b1$solarspots5, phi = 0, bty = "g", type = "b", 
          ticktype = "detailed", pch = 20, 
          cex = c(0.5, 1, 1.5), col = ramp.col(c("black", "yellow", "red")))

library("plot3Drgl")
plotrgl()

cor(b2$prec5, b2$sunspots5)
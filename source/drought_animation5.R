require("ggplot2")
library("RColorBrewer")
library(dplyr)
library(hrbrthemes)
library(viridis)
require("SCI")

do_fft <- function(pic, filterYears) {
  len <- length(pic)
  ## mirror pic
  pic <- append(pic, pic)
  for(i in 1:len) {
    pic[i+len] <- pic[1+len-i]
  }
  frq <- fft(pic, inverse = FALSE)
  frq1 <- frq
  start = round(len/(12*filterYears)) ## monthly
  stop  = round(2*len-start)
  frq1[start:stop] <- 0.0 
  pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
  pic1 <- pic1[1:len]
  return(pic1)
}  

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


hhi_all <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/csv/hhi_1500_2xxx.csv", sep=",")
spi_all <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/csv/spi_1500_2xxx.csv", sep=",")

tempCompl <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/ti_1500_2xxx_monthly.csv", sep=",", na = "NA")
baur <- read.csv("https://raw.githubusercontent.com/climdata/baur/master/csv/baur_monthly.csv", sep=",")
baur2 <- baur[,c('year','month','time', 'ts')]
start <- baur$month[1]
tmp.para <- fitSCI(baur$temperature, first.mon=start,distr="norm",time.scale=1,p0=TRUE)
tmp.spi <- transformSCI(baur$temperature,first.mon=start,obj=tmp.para)
baur2$ti <- signif(tmp.spi, digits=6)
tnew <- subset(baur2, baur2$ts>max(tempCompl$ts))
ti_all <- rbind(tempCompl, tnew)
ti_all <- ti_all[order(ti_all$ts),]


#hhi_all <- hhi_all
txt_droughts <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/data/droughts_txt.csv", sep=",")
txt_wetness <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/data/wetness_txt.csv", sep=",")
txt_and <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/data/and_txt.csv", sep=",")
txt_germany <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/data/germany_txt.csv", sep=",")
txt_1500_2018 <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/data/1500_2018_txt.csv", sep=",")
txt_qr <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/data/qr_code.csv", sep=",")


filterYears <- 1/7
hhi_all$hhi.f <- do_fft(hhi_all$hhi,filterYears)



hhiLimit <- -0.5

hhi_drought <- subset(hhi_all, hhi_all$hhi<hhiLimit)
hhi_drought <- hhi_drought[order(hhi_drought$ts),]

hhi_drought$n <- 0
hhi_drought$hhi.fmax  <- hhi_drought$hhi.f
hhi_drought$hhi.max   <- hhi_drought$hhi
hhi_drought$hhi.sum   <- hhi_drought$hhi
hhi_drought$hhi.avg   <- hhi_drought$hhi
hhi_drought$month.max <- hhi_drought$month
hhi_drought$year.max  <- hhi_drought$year
hhi_drought$ts.max  <- hhi_drought$ts
hhi_drought$ts.start  <- hhi_drought$ts
hhi_drought$ts.stop   <- hhi_drought$ts
hhi_drought$duration  <- 1
hhi_drought$maximum   <- FALSE

n <- 1
for(i in 2:nrow(hhi_drought)) {
  ## check if prevoius month
  if((hhi_drought$ts.start[i]-hhi_drought$ts.stop[i-1]) < 0.1) {
    hhi_drought$n[i] = n
    hhi_drought$n[i-1] = n
    #if(hhi_drought$hhi.fmax[i] < hhi_drought$hhi.fmax[i-1]) {
    if(hhi_drought$hhi.max[i] < hhi_drought$hhi.max[i-1]) {
      hhi_drought$month.max[i-1] <- hhi_drought$month.max[i]
      hhi_drought$year.max[i-1] <- hhi_drought$year.max[i]   
      hhi_drought$ts.max[i-1] <- hhi_drought$ts.max[i]             
      hhi_drought$maximum[i] <- TRUE
      hhi_drought$maximum[i-1] <- FALSE          
    } else {
      hhi_drought$month.max[i] <- hhi_drought$month.max[i-1]
      hhi_drought$year.max[i] <- hhi_drought$year.max[i-1]
      hhi_drought$ts.max[i] <- hhi_drought$ts.max[i-1]          
      #hhi_drought$maximum[i] <- FALSE
      #hhi_drought$maximum[i-1] <- TRUE           
    }
    hhi_drought$hhi.fmax[i] <- min(hhi_drought$hhi.fmax[i],hhi_drought$hhi.fmax[i-1])
    hhi_drought$hhi.max[i] <- min(hhi_drought$hhi.max[i],hhi_drought$hhi.max[i-1])
    hhi_drought$hhi.sum[i] <- (hhi_drought$hhi.sum[i] + hhi_drought$hhi.sum[i-1])
    hhi_drought$duration[i] <- (hhi_drought$duration[i] + hhi_drought$duration[i-1])
    hhi_drought$ts.start[i] <- min(hhi_drought$ts.start[i],hhi_drought$ts.start[i-1])
    hhi_drought$ts.stop[i]  <- max(hhi_drought$ts.stop[i],hhi_drought$ts.stop[i-1])
  } else {
    n <- n+1
    hhi_drought$n[i] = n
  }
}

for(i in nrow(hhi_drought):2) {
  if(hhi_drought$n[i] == hhi_drought$n[i-1]) {
    hhi_drought$hhi.fmax[i-1]  <- min(hhi_drought$hhi.fmax[i],hhi_drought$hhi.fmax[i-1])
    hhi_drought$hhi.max[i-1]   <- min(hhi_drought$hhi.max[i],hhi_drought$hhi.max[i-1])
    hhi_drought$month.max[i-1] <- hhi_drought$month.max[i]
    hhi_drought$year.max[i-1]  <- hhi_drought$year.max[i]
    hhi_drought$ts.max[i-1]  <- hhi_drought$ts.max[i]    
    hhi_drought$duration[i-1]  <- hhi_drought$duration[i]
    hhi_drought$ts.start[i-1]  <- hhi_drought$ts.start[i]
    hhi_drought$ts.stop[i-1]   <- hhi_drought$ts.stop[i]    
    hhi_drought$hhi.sum[i-1]   <- hhi_drought$hhi.sum[i]
    #if(hhi_drought$hhi.f[i-1] > hhi_drought$hhi.fmax[i]) {
    if(hhi_drought$hhi[i-1] > hhi_drought$hhi.max[i]) {
      hhi_drought$maximum[i-1] <- FALSE
    }
  }
}

hhi_drought$hhi.avg <- hhi_drought$hhi.sum / hhi_drought$duration

###
hhi_floods <- subset(hhi_all, hhi_all$hhi>-hhiLimit)
hhi_floods <- hhi_floods[order(hhi_floods$ts),]

hhi_floods$n <- 0
hhi_floods$hhi.fmax  <- hhi_floods$hhi.f
hhi_floods$hhi.max   <- hhi_floods$hhi
hhi_floods$hhi.sum   <- hhi_floods$hhi
hhi_floods$hhi.avg   <- hhi_floods$hhi
hhi_floods$month.max <- hhi_floods$month
hhi_floods$year.max  <- hhi_floods$year
hhi_floods$ts.max  <- hhi_floods$ts
hhi_floods$ts.start  <- hhi_floods$ts
hhi_floods$ts.stop   <- hhi_floods$ts
hhi_floods$duration  <- 1
hhi_floods$maximum   <- FALSE

n <- 1
for(i in 2:nrow(hhi_floods)) {
  ## check if prevoius month
  if((hhi_floods$ts.start[i]-hhi_floods$ts.stop[i-1]) < 0.1) {
    hhi_floods$n[i] = n
    hhi_floods$n[i-1] = n
    #if(hhi_drought$hhi.fmax[i] < hhi_drought$hhi.fmax[i-1]) {
    if(hhi_floods$hhi.max[i] > hhi_floods$hhi.max[i-1]) {
      hhi_floods$month.max[i-1] <- hhi_floods$month.max[i]
      hhi_floods$year.max[i-1] <- hhi_floods$year.max[i]   
      hhi_floods$ts.max[i-1] <- hhi_floods$ts.max[i]             
      hhi_floods$maximum[i] <- TRUE
      hhi_floods$maximum[i-1] <- FALSE          
    } else {
      hhi_floods$month.max[i] <- hhi_floods$month.max[i-1]
      hhi_floods$year.max[i] <- hhi_floods$year.max[i-1]
      hhi_floods$ts.max[i] <- hhi_floods$ts.max[i-1]          
      #hhi_drought$maximum[i] <- FALSE
      #hhi_drought$maximum[i-1] <- TRUE           
    }
    hhi_floods$hhi.fmax[i] <- max(hhi_floods$hhi.fmax[i],hhi_floods$hhi.fmax[i-1])
    hhi_floods$hhi.max[i] <- max(hhi_floods$hhi.max[i],hhi_floods$hhi.max[i-1])
    hhi_floods$hhi.sum[i] <- (hhi_floods$hhi.sum[i] + hhi_floods$hhi.sum[i-1])
    hhi_floods$duration[i] <- (hhi_floods$duration[i] + hhi_floods$duration[i-1])
    hhi_floods$ts.start[i] <- min(hhi_floods$ts.start[i],hhi_floods$ts.start[i-1])
    hhi_floods$ts.stop[i]  <- max(hhi_floods$ts.stop[i],hhi_floods$ts.stop[i-1])
  } else {
    n <- n+1
    hhi_floods$n[i] = n
  }
}

for(i in nrow(hhi_floods):2) {
  if(hhi_floods$n[i] == hhi_floods$n[i-1]) {
    hhi_floods$hhi.fmax[i-1]  <- max(hhi_floods$hhi.fmax[i],hhi_floods$hhi.fmax[i-1])
    hhi_floods$hhi.max[i-1]   <- max(hhi_floods$hhi.max[i],hhi_floods$hhi.max[i-1])
    hhi_floods$month.max[i-1] <- hhi_floods$month.max[i]
    hhi_floods$year.max[i-1]  <- hhi_floods$year.max[i]
    hhi_floods$ts.max[i-1]  <- hhi_floods$ts.max[i]    
    hhi_floods$duration[i-1]  <- hhi_floods$duration[i]
    hhi_floods$ts.start[i-1]  <- hhi_floods$ts.start[i]
    hhi_floods$ts.stop[i-1]   <- hhi_floods$ts.stop[i]    
    hhi_floods$hhi.sum[i-1]   <- hhi_floods$hhi.sum[i]
    #if(hhi_drought$hhi.f[i-1] > hhi_drought$hhi.fmax[i]) {
    if(hhi_floods$hhi[i-1] < hhi_floods$hhi.max[i]) {
      hhi_floods$maximum[i-1] <- FALSE
    }
  }
}

hhi_floods$hhi.avg <- hhi_floods$hhi.sum / hhi_floods$duration

###
hhi_normal <- subset(hhi_all, (abs(hhi_all$hhi)<abs(3*hhiLimit)))  
hhi_normal <- hhi_normal[order(hhi_normal$ts),]

hhi_normal$n <- 0
hhi_normal$hhi.fmax  <- hhi_normal$hhi.f
hhi_normal$hhi.max   <- hhi_normal$hhi
hhi_normal$hhi.sum   <- hhi_normal$hhi
hhi_normal$hhi.avg   <- hhi_normal$hhi
hhi_normal$month.max <- hhi_normal$month
hhi_normal$year.max  <- hhi_normal$year
hhi_normal$ts.max  <- hhi_normal$ts
hhi_normal$ts.start  <- hhi_normal$ts
hhi_normal$ts.stop   <- hhi_normal$ts
hhi_normal$duration  <- 1
hhi_normal$maximum   <- FALSE

n <- 1
for(i in 2:nrow(hhi_normal)) {
  ## check if prevoius month
  if((hhi_normal$ts.start[i]-hhi_normal$ts.stop[i-1]) < 0.1) {
    hhi_normal$n[i] = n
    hhi_normal$n[i-1] = n
    #if(hhi_drought$hhi.fmax[i] < hhi_drought$hhi.fmax[i-1]) {
    if(abs(hhi_normal$hhi.max[i]) < abs(hhi_normal$hhi.max[i-1])) {
      hhi_normal$month.max[i-1] <- hhi_normal$month.max[i]
      hhi_normal$year.max[i-1] <- hhi_normal$year.max[i]   
      hhi_normal$ts.max[i-1] <- hhi_normal$ts.max[i]             
      hhi_normal$maximum[i] <- TRUE
      hhi_normal$maximum[i-1] <- FALSE   
      #hhi_normal$hhi.fmax[i] <- hhi_normal$hhi.fmax[i]
      #hhi_normal$hhi.max[i] <- hhi_normal$hhi.max[i]      
    } else {
      hhi_normal$month.max[i] <- hhi_normal$month.max[i-1]
      hhi_normal$year.max[i] <- hhi_normal$year.max[i-1]
      hhi_normal$ts.max[i] <- hhi_normal$ts.max[i-1]          
      #hhi_drought$maximum[i] <- FALSE
      #hhi_drought$maximum[i-1] <- TRUE
      hhi_normal$hhi.fmax[i] <- hhi_normal$hhi.fmax[i-1]
      hhi_normal$hhi.max[i] <- hhi_normal$hhi.max[i-1]
    }

    hhi_normal$hhi.sum[i] <- (hhi_normal$hhi.sum[i] + hhi_normal$hhi.sum[i-1])
    hhi_normal$duration[i] <- (hhi_normal$duration[i] + hhi_normal$duration[i-1])
    hhi_normal$ts.start[i] <- min(hhi_normal$ts.start[i],hhi_normal$ts.start[i-1])
    hhi_normal$ts.stop[i]  <- max(hhi_normal$ts.stop[i],hhi_normal$ts.stop[i-1])
  } else {
    n <- n+1
    hhi_normal$n[i] = n
  }
}

for(i in nrow(hhi_normal):2) {
  if(hhi_normal$n[i] == hhi_normal$n[i-1]) {

    if(abs(hhi_normal$hhi.max[i]) < abs(hhi_normal$hhi.max[i-1])) {
        
      hhi_normal$hhi.fmax[i-1]  <- hhi_normal$hhi.fmax[i]
      hhi_normal$hhi.max[i-1]   <- hhi_normal$hhi.max[i]
    } 
    #else {
    #  hhi_normal$hhi.fmax[i-1]  <- hhi_normal$hhi.fmax[i-1]
    #  hhi_normal$hhi.max[i-1]   <- hhi_normal$hhi.max[i-1]      
    #}
    
    hhi_normal$month.max[i-1] <- hhi_normal$month.max[i]
    hhi_normal$year.max[i-1]  <- hhi_normal$year.max[i]
    hhi_normal$ts.max[i-1]  <- hhi_normal$ts.max[i]    
    hhi_normal$duration[i-1]  <- hhi_normal$duration[i]
    hhi_normal$ts.start[i-1]  <- hhi_normal$ts.start[i]
    hhi_normal$ts.stop[i-1]   <- hhi_normal$ts.stop[i]    
    hhi_normal$hhi.sum[i-1]   <- hhi_normal$hhi.sum[i]
    #if(hhi_drought$hhi.f[i-1] > hhi_drought$hhi.fmax[i]) {
    if(abs(hhi_normal$hhi[i-1]) > abs(hhi_normal$hhi.max[i])) {
      hhi_normal$maximum[i-1] <- FALSE
    }
  }
}

hhi_normal$hhi.avg <- hhi_normal$hhi.sum / hhi_normal$duration
###


hhi_periods_d <- subset(hhi_drought, hhi_drought$maximum)
hhi_periods_d <- hhi_periods_d[order(hhi_periods_d$hhi.fmax),]
drought_periods <- hhi_periods_d[,c('year','month','time', 'ts', 'hhi.fmax', 'hhi.max', 'hhi.sum', 'hhi.avg', 'duration', 'ts.start', 'ts.stop')]

hhi_periods_f <- subset(hhi_floods, hhi_floods$maximum)
hhi_periods_f <- hhi_periods_f[order(hhi_periods_f$hhi.fmax),]
flood_periods <- hhi_periods_f[,c('year','month','time', 'ts', 'hhi.fmax', 'hhi.max', 'hhi.sum', 'hhi.avg', 'duration', 'ts.start', 'ts.stop')]

hhi_periods_n <- subset(hhi_normal, hhi_normal$maximum)
hhi_periods_n <- hhi_periods_n[order(hhi_periods_n$hhi.fmax),]
normal_periods <- hhi_periods_n[,c('year','month','time', 'ts', 'hhi.fmax', 'hhi.max', 'hhi.sum', 'hhi.avg', 'duration', 'ts.start', 'ts.stop')]

ti_all <- ti_all[,c('year','month','ti')]
spi_all <- spi_all[,c('year','month','spi1')]
pi_ti_all <- merge(spi_all,ti_all, by=c("year","month"))
names(pi_ti_all)[names(pi_ti_all) == "spi1"] <- "pi"

hhi_drought <- merge(hhi_drought,pi_ti_all, by=c("year","month"))
hhi_floods <- merge(hhi_floods,pi_ti_all, by=c("year","month"))
hhi_normal <- merge(hhi_normal,pi_ti_all, by=c("year","month"))

data_max <- max(nrow(hhi_drought),nrow(hhi_floods),nrow(hhi_normal))

for (i in (nrow(txt_droughts)+1):data_max) {
  txt_droughts <- rbind(txt_droughts, data.frame(x = 0, y = 0))
}
for (i in (nrow(txt_wetness)+1):data_max) {
  txt_wetness <- rbind(txt_wetness, data.frame(x = 0, y = 0))
}
for (i in (nrow(txt_and)+1):data_max) {
  txt_and <- rbind(txt_and, data.frame(x = 0, y = 1))
}
for (i in (nrow(txt_germany)+1):data_max) {
  txt_germany <- rbind(txt_germany, data.frame(x = 0, y = 2))
}
for (i in (nrow(txt_1500_2018)+1):data_max) {
  txt_1500_2018 <- rbind(txt_1500_2018, data.frame(x = 0, y = 2))
}
for (i in (nrow(txt_qr)+1):data_max) {
  txt_qr <- rbind(txt_qr, data.frame(x = 0, y = 0))
}
if(nrow(hhi_drought) < data_max) {
for (i in (nrow(hhi_drought)+1):data_max) {
  hhi_drought <- rbind(hhi_drought, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE, pi=0, ti=0))
}
}
if(nrow(hhi_floods) < data_max) {
for (i in (nrow(hhi_floods)+1):data_max) {
  hhi_floods <- rbind(hhi_floods, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE, pi=0, ti=0))
}
}
if(nrow(hhi_normal) < data_max) {
  for (i in (nrow(hhi_normal)+1):data_max) {
    hhi_normal <- rbind(hhi_normal, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE, pi=0, ti=0))
  }
}





#5800
##for(yearStart4 in 5800:8080) {
for(yearStart4 in 5200:8080) {
  
    
  #yearStart4 <- 7920
   yearStart <- yearStart4/4
   yearEnd <- yearStart+100

   #precColors = 
   tempColors = rev(brewer.pal(n = 7, name = "RdBu"))
   droughtColors = brewer.pal(n = 5, name = "YlOrRd")
   floodColors = brewer.pal(n = 5, name = "GnBu")
   fdColors <-  c(rev(droughtColors), "grey", floodColors)   

   
   mp <- ggplot(hhi_drought, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
   mp +
     #geom_raster(aes(fill=-hhi))+
     geom_tile(aes(fill=pi, width=1, height=1))+
     geom_tile(aes(x=txt_droughts$x+1414, y=34-txt_droughts$y, width=1, height=1, fill=-3))+
     geom_tile(aes(x=txt_germany$x+1413, y=23-txt_germany$y, width=1, height=1, fill=0))+
     geom_tile(aes(x=txt_1500_2018$x+1411, y=11-txt_1500_2018$y, width=1, height=1, fill=3))+
     geom_tile(aes(x=txt_qr$x+1460, y=33-txt_qr$y, width=1, height=1, fill=99))+
     theme_classic(base_size=80) +
     #theme_classic() +
     labs(x="Year", y="Month") +
     #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
     scale_y_continuous(breaks=c(0,6,12,18,24,30,36), limits=c(-1,37))+
     scale_x_continuous(limits=c(yearStart,yearEnd), breaks = seq(from = 1300, to = 2220, by = 20)) + 
     scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30",
                          limits=c(-3,3)) +
     theme( legend.key.width = unit(2,"cm")) +
     guides(fill=guide_legend(title="PI", reverse = TRUE))
   
   fileName5 <- paste('drought_begin_pi_', toString(yearStart4), '.png', sep='')
   #ggsave(fileName5, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)
   ggsave(fileName5, width = 32, height = 18, limitsize=FALSE, device='png', dpi=96)   
   

   mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
   mp +
     #geom_raster(aes(fill=-hhi))+
     geom_tile(aes(fill=hhi_floods$hhi, width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
     geom_tile(aes(fill=hhi_drought$hhi, width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
     geom_tile(aes(fill=hhi_normal$hhi, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
     geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=4))+
     geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
     geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-4))+
     geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
     geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
     geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
     theme_classic(base_size=80) +
     #theme_classic() +
     labs(x="Year", y="Duration [months]") +
     #labs(x="Year", y="Duration [months]", title="", subtitle="") +
     #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
     scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','N:6','N:0','N:6','W:12','W:6','W:0'),limits=c(-3,51))+
     scale_x_continuous(limits=c(yearStart,yearEnd), breaks = seq(from = 1300, to = 2220, by = 20)) + 
     scale_fill_gradientn(colors=fdColors, limits=c(-4,4), breaks=c(-4,-3,-2,-1,0,1,2,3,4)) + 
     theme( legend.key.width = unit(2,"cm")) +
     guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName8 <- paste('triple_begin_', toString(yearStart4), '.png', sep='')
#ggsave(fileName8, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)
ggsave(fileName8, width = 32, height = 18, limitsize=FALSE, device='png', dpi=96)

mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi_floods$ti, width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_drought$ti, width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
  #geom_tile(aes(fill=hhi_normal$ti, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=99))+
  geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
  geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=99))+
  geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','D:18','','W:18','W:12','W:6','W:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(yearStart,yearEnd), breaks = seq(from = 1300, to = 2220, by = 20)) + 
  scale_fill_gradientn(colors=tempColors, limits=c(-3,3), breaks=c(-3,-2,-1,0,1,2,3)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="TI", reverse = TRUE))

fileName9b <- paste('both_begin_ti_', toString(yearStart4), '.png', sep='')
ggsave(fileName9b, width = 32, height = 18, limitsize=FALSE, device='png', dpi=96)


mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi_floods$pi, width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_drought$pi, width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
  #geom_tile(aes(fill=hhi_normal$pi, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=3))+
  geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
  geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-3))+
  geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','D:18','','W:18','W:12','W:6','W:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(yearStart,yearEnd), breaks = seq(from = 1300, to = 2220, by = 20)) + 
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30",
                       limits=c(-3,3)) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="PI", reverse = TRUE))

fileName10 <- paste('both_begin_pi_', toString(yearStart4), '.png', sep='')
ggsave(fileName10, width = 32, height = 18, limitsize=FALSE, device='png', dpi=96)



}

?ggsave




droughtColors = brewer.pal(n = 5, name = "YlOrRd")

mp <- ggplot(hhi_drought, aes(year.max, round(12*(ts+1/24))-round(6*(ts.start+ts.stop+1/12))))
mp + 
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=-hhi, width=1, height=1))+
  geom_tile(aes(x=txt_droughts$x+1402, y=16-txt_droughts$y, width=1, height=1, fill=6))+
  geom_tile(aes(x=txt_germany$x+1403, y=5-txt_germany$y, width=1, height=1, fill=4))+
  geom_tile(aes(x=txt_1500_2018$x+1401, y=-7-txt_1500_2018$y, width=1, height=1, fill=2))+
  geom_tile(aes(x=txt_qr$x+1460, y=15-txt_qr$y, width=1, height=1, fill=-1))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(-18,-12,-6,0,6,12,18), limits=c(-20,20))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradientn(colors=droughtColors, limits=c(0,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))


fileName1 <- 'period_centered_all.png'
ggsave(fileName1, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

mp <- ggplot(hhi_drought, aes(year.max, round(12*(ts-ts.max))))
mp + 
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=-hhi, width=1, height=1))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(-18,-12,-6,0,6,12,18), limits=c(-20,20))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradientn(colors=droughtColors, limits=c(0,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))


fileName1 <- 'period_centered_max.png'
ggsave(fileName1, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

mp <- ggplot(hhi_drought, aes(x=year, y=month))
mp + 
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=-hhi, width=1, height=1))+
  theme_classic(base_size=80) +
  #  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(3,6,9,12), limits=c(1,12))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradientn(colors=droughtColors) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName5 <- paste('period_year_all.png', sep='')
ggsave(fileName5, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)



mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi_floods$hhi, width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_drought$hhi, width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_normal$hhi, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=4))+
  geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
  geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-4))+
  geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','N:6','N:0','N:-6','W:12','W:6','W:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(1500,2020)) + 
  scale_fill_gradientn(colors=fdColors, limits=c(-4,4), breaks=c(-4,-3,-2,-1,0,1,2,3,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName7 <- paste('both_begin_all.png', sep='')
ggsave(fileName7, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)




mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=mixTPColor(hhi_floods$ti/3, hhi_floods$pi/3), width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=mixTPColor(hhi_drought$ti/3, hhi_drought$pi/3), width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
  geom_tile(aes(fill=mixTPColor(hhi_normal$ti/3, hhi_normal$pi/3), width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  #geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=4))+
  #geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
  #geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-4))+
  #geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  #geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  #geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) + theme(legend.position = "none") +
  #theme_classic() +
  labs(x="Year", y="Month") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','N:6','N:0','N:-6','W:12','W:6','W:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(1500,2020)) + 
  #scale_fill_gradientn(colors=fdColors, limits=c(-4,4), breaks=c(-4,-3,-2,-1,0,1,2,3,4)) + 
  #theme( legend.key.width = unit(2,"cm")) +
  #guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName7 <- paste('both_begin_pt.png', sep='')
ggsave(fileName7, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)



mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi_floods$ti, width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_drought$ti, width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
  #geom_tile(aes(fill=hhi_normal$ti, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=3))+
  geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=99))+
  geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-3))+
  geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','N:6','N:0','N:-6','W:12','W:6','W:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(1500,2020)) + 
  scale_fill_gradientn(colors=tempColors, limits=c(-3,3), breaks=c(-3,-2,-1,0,1,2,3)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="TI", reverse = TRUE))
  
  fileName7 <- paste('both_begin_t.png', sep='')
ggsave(fileName7, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)


mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi_floods$pi, width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_drought$pi, width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
  #geom_tile(aes(fill=hhi_normal$pi, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=3))+
  geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
  geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-3))+
  geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','N:6','N:0','N:-6','W:12','W:6','W:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(1500,2020)) + 
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30",
                       limits=c(-3,3)) +
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="PI", reverse = TRUE))

fileName7 <- paste('both_begin_p.png', sep='')
ggsave(fileName7, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(6*(ts.start+ts.stop+1/12))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi, width=1, height=1))+
  ##geom_tile(aes(x=txt_droughts$x+1402, y=16-txt_droughts$y, width=1, height=1, fill=6))+
  geom_tile(aes(x=txt_germany$x+1403, y=5-txt_germany$y, width=1, height=1, fill=4))+
  geom_tile(aes(x=txt_1500_2018$x+1401, y=-7-txt_1500_2018$y, width=1, height=1, fill=2))+
  geom_tile(aes(x=txt_qr$x+1460, y=15-txt_qr$y, width=1, height=1, fill=-1))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(-18,-12,-6,0,6,12,18), limits=c(-20,20))+
  scale_x_continuous(limits=c(1400,1500)) + 
  scale_fill_gradientn(colors=floodColors, limits=c(0,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName6 <- paste('flood_centered_all.png', sep='')
ggsave(fileName6, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)


library(tsbox)
ts_tsibble(hiFull[,c('time', 'hhi')])
ts_plot(ts_trend(hiFull[,c('time', 'hhi')]))
ts_plot(ts_compound(hiFull[,c('time', 'hhi')]))
ts_plot(ts_frequency(hiFull[,c('time', 'hhi')]))


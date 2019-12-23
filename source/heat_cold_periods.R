require("ggplot2")
library("RColorBrewer")
library(dplyr)
library(hrbrthemes)
library(viridis)
library("zoo")
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


tempCompl <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/ti_1500_2xxx_monthly.csv", sep=",", na = "NA")

baur <- read.csv("https://raw.githubusercontent.com/climdata/baur/master/csv/baur_monthly.csv", sep=",")
baur2 <- baur[,c('year','month','time', 'ts')]
start <- baur$month[1]
for (m in c(1,2,3,4,5,6,7,8,9,10,11,12)) {
  tmp.para <- fitSCI(baur$temperature, first.mon=start,distr="norm",time.scale=m,p0=TRUE)
  tmp.spi <- transformSCI(baur$temperature,first.mon=start,obj=tmp.para)
  baur2$new <- signif(tmp.spi, digits=6)
  names(baur2)[names(baur2) == "new"] <- paste("sti", m, sep="")
}

hi <- tempCompl
hi$hti1 <- hi$ti
hi <- hi[order(hi$ts),]
prev <- hi$hti1
for (m in c(2,3,4,5,6,7,8,9,10,11,12)) {
  column <- paste("hti", m, sep="")
  hti <- rollapply(hi$ti, width=m, by=1, FUN=sum)
  hi$hti <- prev
  hi$hti[m:length(hi$hti)] <- hti
  prev <- hi$hti
  names(hi)[names(hi) == 'hti'] <- column
}

hsti <- hi[,c('year','month','time', 'ts')]
for (m in c(1:12)) {
  hdiCol <- paste("hti", m, sep="")
  spiCol <- paste("sti", m, sep="")
  sti <- hi[,hdiCol] * m^(-1/sqrt(3.0))
  # more exact reconstruction
  hsti$sti <- round(sti, digits=6)
  names(hsti)[names(hsti) == 'sti'] <- spiCol
}

tnew <- subset(baur2, baur2$ts>max(hsti$ts))
hsti <- rbind(hsti, tnew)
hsti <- hsti[order(hsti$ts),]


extremeLimit <- 4.0  ## goes up to 5
hiFull <- hsti

## Historical Drought Index HDI
hiFull$hdi1 <- hiFull$sti1
hiFull$hdi2 <- hiFull$sti2
hiFull$hdi3 <- hiFull$sti3
hiFull$hdi4 <- hiFull$sti4
hiFull$hdi5 <- hiFull$sti5
hiFull$hdi <- hiFull$sti12 
for(i in 1:nrow(hiFull)) {
  hiFull$hdi1[i] <- max(-1.0,min(hiFull$sti1[i], hiFull$sti2[i], 0.0, na.rm = TRUE))
  
  hiFull$hdi2[i] <- max(-1.5,min(hiFull$sti1[i], hiFull$sti2[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi2[i]>-1.0) {hiFull$hdi2[i] = 0.0}
  
  hiFull$hdi3[i] <- max(-2.0,min(hiFull$sti2[i], hiFull$sti3[i], hiFull$sti4[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi3[i]>-1.5) {hiFull$hdi3[i] = 0.0}  
  
  hiFull$hdi4[i] <- max(-3.0,min(hiFull$spi4[i], hiFull$sti5[i], hiFull$sti6[i],hiFull$sti7[i], hiFull$sti8[i], hiFull$sti9[i],hiFull$sti10[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi4[i]>-2.0) {hiFull$hdi4[i] = 0.0}
  
  hiFull$hdi5[i] <- max(-extremeLimit,min(hiFull$sti10[i], hiFull$sti11[i], hiFull$sti12[i], 1.0, na.rm = TRUE))
  if (hiFull$hdi5[i]>-3.0) {hiFull$hdi5[i] = 0.0}   
  
  hiFull$hdi[i] <- min(hiFull$hdi1[i],hiFull$hdi2[i],hiFull$hdi3[i],hiFull$hdi4[i],hiFull$hdi5[i], 0.0, na.rm = TRUE)
}
## Historical Wet Index HWI
hiFull$hwi1 <- hiFull$sti1
hiFull$hwi2 <- hiFull$sti2
hiFull$hwi3 <- hiFull$sti3
hiFull$hwi4 <- hiFull$sti4
hiFull$hwi5 <- hiFull$sti5
hiFull$hwi <- hiFull$sti12  
hiFull$hdwi <- hiFull$spi12  ## Drought/Wet Index => Difference
hiFull$hhi <- hiFull$spi12  ## Humidity Index  => Maximum Extreme -> Larger Amplitude
for(i in 1:nrow(hiFull)) {
  hiFull$hwi1[i] <- min(1.0,max(hiFull$sti1[i], hiFull$sti2[i], 0.0, na.rm = TRUE))
  
  hiFull$hwi2[i] <- min(1.5,max(hiFull$sti1[i], hiFull$sti2[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi2[i]<1.0) {hiFull$hwi2[i] = 0.0}
  
  hiFull$hwi3[i] <- min(2.0,max(hiFull$sti2[i], hiFull$sti3[i], hiFull$sti4[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi3[i]<1.5) {hiFull$hwi3[i] = 0.0}  
  
  hiFull$hwi4[i] <- min(3.0,max(hiFull$sti4[i], hiFull$sti5[i], hiFull$sti6[i],hiFull$sti7[i], hiFull$sti8[i], hiFull$sti9[i],hiFull$sti10[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi4[i]<2.0) {hiFull$hwi4[i] = 0.0}
  
  hiFull$hwi5[i] <- min(extremeLimit,max(hiFull$sti10[i], hiFull$sti11[i], hiFull$sti12[i], -1.0, na.rm = TRUE))
  if (hiFull$hwi5[i]<3.0) {hiFull$hwi5[i] = 0.0}   
  
  hiFull$hwi[i] <- max(hiFull$hwi1[i],hiFull$hwi2[i],hiFull$hwi3[i],hiFull$hwi4[i],hiFull$hwi5[i], 0.0, na.rm = TRUE)
  hiFull$hdwi[i] <- hiFull$hwi[i] + hiFull$hdi[i]
  if (hiFull$hdwi[i] < 0) {
    hiFull$hhi[i] <- hiFull$hdi[i]
  } else {
    hiFull$hhi[i] <- hiFull$hwi[i]
  }
  
}
hhi <- hiFull[,c('year','month','time', 'ts', 'hdi', 'hwi', 'hhi', 'hdwi')]
hhi$hdi <- round(hhi$hdi, digits=6)
hhi$hwi <- round(hhi$hwi, digits=6)
hhi$hhi <- round(hhi$hhi, digits=6)
hhi$hdwi <- round(hhi$hdwi, digits=6)

#write.table(hhi, file = "csv/hti_1500_2xxx.csv", append = FALSE, quote = TRUE, sep = ",",
#            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
#            col.names = TRUE, qmethod = "escape", fileEncoding = "UTF-8")


hhiLimit <- -0.5
filterYears <- 1/7  # ~2 month   ##  

hhi_all <- hhi

##
#hhi_all <- tempCompl
#names(hhi_all)[names(hhi_all) == 'ti'] <- 'hhi'
#hhi_all$hdi <- min(hhi_all$hhi, 0)
#hhi_all$hwi <- max(hhi_all$hhi, 0)
#hhi_all$hdwi <- hhi_all$hhi
##

hhi_all$hhi.f <- do_fft(hhi_all$hhi,filterYears)

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


ti_all <- hsti[,c('year','month','sti1')]
names(ti_all)[names(ti_all) == "sti1"] <- "ti"

hhi_drought <- merge(hhi_drought,ti_all, by=c("year","month"))
hhi_floods <- merge(hhi_floods,ti_all, by=c("year","month"))
hhi_normal <- merge(hhi_normal,ti_all, by=c("year","month"))


data_max <- max(nrow(hhi_drought),nrow(hhi_floods),nrow(hhi_normal))

#for (i in (nrow(txt_droughts)+1):data_max) {
#  txt_droughts <- rbind(txt_droughts, data.frame(x = 0, y = 0))
#}
#for (i in (nrow(txt_wetness)+1):data_max) {
#  txt_wetness <- rbind(txt_wetness, data.frame(x = 0, y = 0))
#}
#for (i in (nrow(txt_and)+1):data_max) {
#  txt_and <- rbind(txt_and, data.frame(x = 0, y = 1))
#}
#for (i in (nrow(txt_germany)+1):data_max) {
#  txt_germany <- rbind(txt_germany, data.frame(x = 0, y = 2))
#}
#for (i in (nrow(txt_1500_2018)+1):data_max) {
#  txt_1500_2018 <- rbind(txt_1500_2018, data.frame(x = 0, y = 2))
#}
#for (i in (nrow(txt_qr)+1):data_max) {
#  txt_qr <- rbind(txt_qr, data.frame(x = 0, y = 0))
#}
if(nrow(hhi_drought) < data_max) {
  for (i in (nrow(hhi_drought)+1):data_max) {
    hhi_drought <- rbind(hhi_drought, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE, ti=0))
  }
}
if(nrow(hhi_floods) < data_max) {
  for (i in (nrow(hhi_floods)+1):data_max) {
    hhi_floods <- rbind(hhi_floods, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE, ti=0))
  }
}
if(nrow(hhi_normal) < data_max) {
  for (i in (nrow(hhi_normal)+1):data_max) {
    hhi_normal <- rbind(hhi_normal, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE, ti=0))
  }
}

tempColors = rev(brewer.pal(n = 7, name = "RdBu"))
heatColors = brewer.pal(n = 5, name = "YlOrRd")
coldColors = brewer.pal(n = 5, name = "GnBu")
hcColors <-  c(rev(floodColors), "grey", droughtColors)   

mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi_floods$hhi, width=1, height=1, x=hhi_floods$year.max, y=round(12*(hhi_floods$ts+1/24))-round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_drought$hhi, width=1, height=1, x=hhi_drought$year.max, y=48-round(12*(hhi_drought$ts+1/24))+round(12*(hhi_drought$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_normal$hhi, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  #geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=4))+
  #geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
  #geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-4))+
  #geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  #geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  #geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('H:0','H:6','H:12','N:6','N:0','N:-6','C:12','C:6','C:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(1500,2020)) + 
  scale_fill_gradientn(colors=hcColors, limits=c(-4,4), breaks=c(-4,-3,-2,-1,0,1,2,3,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HTI", reverse = TRUE))

fileName7 <- paste('temp_begin_all.png', sep='')
ggsave(fileName7, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
mp +
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi_floods$ti, width=1, height=1, x=hhi_floods$year.max, y=round(12*(hhi_floods$ts+1/24))-round(12*(hhi_floods$ts.start+1/24))))+
  geom_tile(aes(fill=hhi_drought$ti, width=1, height=1, x=hhi_drought$year.max, y=48-round(12*(hhi_drought$ts+1/24))+round(12*(hhi_drought$ts.start+1/24))))+
  ##geom_tile(aes(fill=hhi_normal$ti, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
  #geom_tile(aes(x=txt_wetness$x+1406, y=49-txt_wetness$y, width=1, height=1, fill=4))+
  #geom_tile(aes(x=txt_and$x+1411, y=28-txt_and$y, width=1, height=1, fill=0))+
  #geom_tile(aes(x=txt_droughts$x+1406, y=8-txt_droughts$y, width=1, height=1, fill=-4))+
  #geom_tile(aes(x=txt_germany$x+1455, y=49-txt_germany$y, width=1, height=1, fill=2))+
  #geom_tile(aes(x=txt_1500_2018$x+1453, y=8-txt_1500_2018$y, width=1, height=1, fill=-2))+
  #geom_tile(aes(x=txt_qr$x+1435, y=39-txt_qr$y, width=1, height=1, fill=99))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Months", title="", subtitle="") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('H:0','H:6','H:12','H:18','','C:18','C:12','C:6','C:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(1500,2020)) + 
  scale_fill_gradientn(colors=tempColors, limits=c(-3,3), breaks=c(-3,-2,-1,0,1,2,3)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="TI", reverse = TRUE))

fileName7b <- paste('temp_begin_ti.png', sep='')
ggsave(fileName7b, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

##??
mp <- ggplot(hhi_floods, aes(year.max, month.max+round(12*(ts-ts.max))))
mp + 
  #geom_raster(aes(fill=-hhi))+
  geom_tile(aes(fill=hhi, width=1, height=1))+
  #geom_tile(aes(x=txt_droughts$x+1404, y=22-txt_droughts$y, width=1, height=1, fill=6))+
  #geom_tile(aes(x=txt_germany$x+1408, y=10-txt_germany$y, width=1, height=1, fill=4))+
  #geom_tile(aes(x=txt_1500_2018$x+1412, y=-3-txt_1500_2018$y, width=1, height=1, fill=2))+
  #geom_tile(aes(x=txt_qr$x+1460, y=21-txt_qr$y, width=1, height=1, fill=-1))+
  theme_classic(base_size=80) +
  #  #theme_classic() +
  labs(x="Year", y="Month") +
  scale_y_continuous(breaks=c(3,6,9,12), limits=c(-12,24), labels=c('mar','jun','sep','dec'))+
  scale_x_continuous(limits=c(1500,2020), breaks = seq(from = 1300, to = 2220, by = 20)) +  
  scale_fill_gradientn(colors=droughtColors, breaks=c(0,1,2,3,4), labels=c('0','1','2','3','4'), limit=c(0,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))


fileName2 <- paste('heat_season_all.png', sep='')
ggsave(fileName2, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)
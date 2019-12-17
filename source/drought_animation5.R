require("ggplot2")
library("RColorBrewer")
library(dplyr)
library(hrbrthemes)
library(viridis)


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

hhi_all <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/csv/hhi_1500_2xxx.csv", sep=",")
#hhi_orig <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/03a18c900d0000e2c98697764910b6b9f1bef32a/csv/hhi_1500_2xxx.csv", sep=",")

hhi_all <- hhi_all

txt_droughts <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/data/droughts_txt.csv", sep=",")
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


data_max <- max(nrow(hhi_drought),nrow(hhi_floods),nrow(hhi_normal))

for (i in (nrow(txt_droughts)+1):data_max) {
  txt_droughts <- rbind(txt_droughts, data.frame(x = 0, y = 0))
}
for (i in (nrow(txt_germany)+1):data_max) {
  txt_germany <- rbind(txt_germany, data.frame(x = 0, y = 0))
}
for (i in (nrow(txt_1500_2018)+1):data_max) {
  txt_1500_2018 <- rbind(txt_1500_2018, data.frame(x = 0, y = 0))
}
for (i in (nrow(txt_qr)+1):data_max) {
  txt_qr <- rbind(txt_qr, data.frame(x = 0, y = 0))
}
if(nrow(hhi_drought) < data_max) {
for (i in (nrow(hhi_drought)+1):data_max) {
  hhi_drought <- rbind(hhi_drought, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE))
}
}
if(nrow(hhi_floods) < data_max) {
for (i in (nrow(hhi_floods)+1):data_max) {
  hhi_floods <- rbind(hhi_floods, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE))
}
}
if(nrow(hhi_normal) < data_max) {
  for (i in (nrow(hhi_normal)+1):data_max) {
    hhi_normal <- rbind(hhi_normal, data.frame(year=0,month=0,time='1000-0-0', ts=0.0, hdi=0.0, hwi=0.0, hhi=0.0, hdwi=0.0, hhi.f=0.0, n=0, hhi.fmax=0.0, hhi.max=0.0, hhi.sum=0.0, hhi.avg=0.0, month.max=0, year.max=0, ts.max=0.0, ts.start=0.0, ts.stop=0.0, duration=0, maximum=FALSE))
  }
}

#5800
##for(yearStart4 in 5800:8080) {
for(yearStart4 in 5400:8080) {
  
    
  #yearStart4 <- 7920
   yearStart <- yearStart4/4
   yearEnd <- yearStart+50

   droughtColors = brewer.pal(n = 5, name = "YlOrRd")
   floodColors = brewer.pal(n = 5, name = "GnBu")
   fdColors <-  c(rev(droughtColors), "grey", floodColors)   


   mp <- ggplot(hhi_floods, aes(year.max, round(12*(ts+1/24))-round(12*(ts.start+1/24))))
   mp +
     #geom_raster(aes(fill=-hhi))+
     geom_tile(aes(fill=hhi_floods$hhi, width=1, height=1, x=hhi_floods$year.max, y=48-round(12*(hhi_floods$ts+1/24))+round(12*(hhi_floods$ts.start+1/24))))+
     geom_tile(aes(fill=hhi_drought$hhi, width=1, height=1, x=hhi_drought$year.max, y=round(12*(hhi_drought$ts+1/24))-round(12*(hhi_drought$ts.start+1/24))))+
     geom_tile(aes(fill=hhi_normal$hhi, width=1, height=1, x=hhi_normal$year.max, y=24+round(12*(hhi_normal$ts+1/24))-round(6*(hhi_normal$ts.start+hhi_normal$ts.stop+1/12))))+
     geom_tile(aes(x=txt_droughts$x+1402, y=42-txt_droughts$y, width=1, height=1, fill=99))+
     geom_tile(aes(x=txt_germany$x+1403, y=30-txt_germany$y, width=1, height=1, fill=-4))+
     geom_tile(aes(x=txt_1500_2018$x+1401, y=18-txt_1500_2018$y, width=1, height=1, fill=-2))+
     geom_tile(aes(x=txt_qr$x+1460, y=40-txt_qr$y, width=1, height=1, fill=4))+
     theme_classic(base_size=80) +
     #theme_classic() +
     labs(x="Year", y="Month", title="", subtitle="") +
     #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
     scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','N:6','N:0','N:6','W:12','W:6','W:0'),limits=c(-3,51))+
     scale_x_continuous(limits=c(yearStart,yearEnd)) + 
     scale_fill_gradientn(colors=fdColors, limits=c(-4,4)) + 
     theme( legend.key.width = unit(2,"cm")) +
     guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName8 <- paste('both_begin_', toString(yearStart4), '.png', sep='')
ggsave(fileName8, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)
}




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
  geom_tile(aes(x=txt_droughts$x+1402, y=42-txt_droughts$y, width=1, height=1, fill=99))+
  geom_tile(aes(x=txt_germany$x+1403, y=30-txt_germany$y, width=1, height=1, fill=-4))+
  geom_tile(aes(x=txt_1500_2018$x+1401, y=18-txt_1500_2018$y, width=1, height=1, fill=-2))+
  geom_tile(aes(x=txt_qr$x+1460, y=40-txt_qr$y, width=1, height=1, fill=4))+
  theme_classic(base_size=80) +
  #theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  #labs(x="Year", y="Month", title="", subtitle="10.5194/cp-2019-104, tambora.org") +
  scale_y_continuous(breaks=c(0,6,12,18,24,30,36,42,48), labels=c('D:0','D:6','D:12','N:6','N:0','N:6','W:12','W:6','W:0'),limits=c(-3,51))+
  scale_x_continuous(limits=c(1500,2020)) + 
  scale_fill_gradientn(colors=fdColors, limits=c(-4,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName7 <- paste('both_begin_all.png', sep='')
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
  scale_x_continuous(limits=c(1500,2020)) + 
  scale_fill_gradientn(colors=floodColors, limits=c(0,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

fileName6 <- paste('flood_centered_all.png', sep='')
ggsave(fileName6, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

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

filterYears <- 1/6
hhi_all$hhi.f <- do_fft(hhi_all$hhi,filterYears)


for(limit in 0:40) {
hhiLimit <- -limit/10

hhi_drought <- subset(hhi_all, hhi_all$hhi.f<hhiLimit)
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
    if(hhi_drought$hhi.fmax[i] < hhi_drought$hhi.fmax[i-1]) {
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
    if(hhi_drought$hhi.f[i-1] > hhi_drought$hhi.fmax[i]) {
      hhi_drought$maximum[i-1] <- FALSE
    }
  }
}

hhi_drought$hhi.avg <- hhi_drought$hhi.sum / hhi_drought$duration

hhi_periods <- subset(hhi_drought, hhi_drought$maximum)
hhi_periods <- hhi_periods[order(hhi_periods$hhi.fmax),]

drought_periods <- hhi_periods[,c('year','month','time', 'ts', 'hhi.fmax', 'hhi.max', 'hhi.sum', 'hhi.avg', 'duration', 'ts.start', 'ts.stop')]




droughtColors = brewer.pal(n = 5, name = "YlOrRd")
limitText <- paste('Limit:', toString(-hhiLimit))

ggplot(data=hhi_periods, aes(y=-hhi.fmax, x=year, size=duration, color=-hhi.avg, label=year)) +
  theme_classic(base_size=80) +
  theme( legend.key.width = unit(2,"cm"), legend.key.height = unit(4,"cm")) +
  guides(fill=guide_legend(title="Droughts", reverse = TRUE)) +
  xlab("Year") + ylab("HHI max") +
  scale_x_continuous(breaks=c(1500,1600,1700,1800,1900,2000), limits=c(1500,2020)) +
  ##scale_y_continuous(breaks=c(0,1,2,3,4), limits=c(0,4.5)) +  
  labs(caption=limitText)+
  geom_text(alpha=0.8, check_overlap = FALSE)+
  scale_size(range = c(6, 24), name="Duration", trans="log", breaks=c(1,2,5,10,20))+
  scale_color_gradientn(colors=droughtColors, limits=c(0,4), name="HHI Ø", breaks=c(0,1,2,3,4))

fileName <- paste('droughts_', toString(10+limit), '.png', sep='')

ggsave(fileName, width = 50, height = 25, limitsize=FALSE, device='png', dpi=96)

fileName2 <- paste('droughts_', toString(99-limit), '.png', sep='')
ggsave(fileName2, width = 50, height = 25, limitsize=FALSE, device='png', dpi=96)

}
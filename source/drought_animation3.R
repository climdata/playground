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



hhiLimit <- -1.2

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

for(yearStart in 1500:1970) {
  
  
  #yearStart <- 1515
   yearEnd <- yearStart+50

droughtColors = brewer.pal(n = 5, name = "YlOrRd")
yearText <- paste('Years:', toString(yearStart), '-', yearEnd)



hhi_periods2 <- subset(hhi_periods, ((hhi_periods$year >= yearStart) & (hhi_periods$year <= yearEnd)))

hhi_periods2$hhi.factor <- (50-abs(2*hhi_periods2$year-yearStart-yearEnd))/50
hhi_periods2$hhi.alpha  <- 0.0+1.0*hhi_periods2$hhi.factor

monthColors = c("blue","green", "red","brown", "blue")
ggplot(data=hhi_periods2, aes(x=duration, y=-hhi.avg, size=-hhi.fmax, color=month.max, label=year, alpha=hhi.alpha)) +
  ##geom_point(alpha=0.5) +
  geom_text(check_overlap = FALSE)+
  theme_classic(base_size=80) +
  theme( legend.key.width = unit(2,"cm"), legend.key.height = unit(4,"cm")) +
  guides(size=guide_legend(order=1), color=guide_colorbar(order=2))+
  xlab("Duration") + ylab("HHI Ø") +
  scale_x_continuous(trans='log', limit = c(1, 35), breaks=c(1,2,5,10,20)) +
  scale_y_continuous(limit = c(1, 4), breaks=c(0,1,2,3,4)) +
  scale_color_gradientn(colors=monthColors, name="Month", limit = c(1, 12), breaks=c(3,6,9,12))+
  scale_alpha_continuous(guide = 'none') +
  scale_size(range = c(6, 50), name="HHI max", limit = c(0.5, 4.5), breaks=c(0,1,2,3,4,5))+
  labs(caption=yearText)


fileName1 <- paste('droughts_', toString(yearStart), '.png', sep='')
ggsave(fileName1, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

fileName2 <- paste('droughts_', toString(5000-yearStart), '.png', sep='')
ggsave(fileName2, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)


hhi_drought2 <- subset(hhi_drought, ((hhi_drought$year.max >= yearStart) & (hhi_drought$year.max <= yearEnd)))

hhi_drought2$hhi.factor <- (50-abs(2*hhi_drought2$year.max-yearStart-yearEnd))/50
hhi_drought2$hhi.alpha  <- 0.0+1.0*hhi_drought2$hhi.factor

droughtColors = brewer.pal(n = 5, name = "YlOrRd")
ggplot(hhi_drought2, aes(group=n, color=duration, y=-hhi.f, x=12*(ts-ts.max), alpha=hhi.alpha))+
  theme_classic(base_size=80) +
  theme( legend.key.width = unit(2,"cm"), legend.key.height = unit(4,"cm")) +
  geom_path(size=4)+
  #guides(color=guide_colorbar(order=2))+
  #stat_smooth(y ~ s(x, k = 24), method = "gam", se = FALSE)+
  scale_x_continuous(limits=c(-7,7), breaks=c(-6,-3,0,3,6)) + 
  scale_y_continuous(limit = c(1, 5), breaks=c(0,1,2,3,4)) +
  xlab("Rel. Time to Max") + ylab("HHI max") +
  scale_color_gradientn(colors=droughtColors, trans='log', name='Duration', limit= c(1,50), breaks=c(1,2,5,10,20))+
  scale_alpha_continuous(guide = 'none') +
  labs(caption=yearText)

fileName3 <- paste('track_', toString(yearStart), '.png', sep='')
ggsave(fileName3, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)

fileName4 <- paste('track_', toString(5000-yearStart), '.png', sep='')
ggsave(fileName4, width = 40, height = 25, limitsize=FALSE, device='png', dpi=96)


}
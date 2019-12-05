require("ggplot2")
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

hhiLimit <- -0.7

hhi_drought <- subset(hhi_all, hhi_all$hhi<hhiLimit)
hhi_drought <- hhi_drought[order(hhi_drought$ts),]

hhi_drought$n <- 0
hhi_drought$hhi.fmax  <- hhi_drought$hhi.f
hhi_drought$hhi_max   <- hhi_drought$hhi
hhi_drought$hhi_sum   <- hhi_drought$hhi
hhi_drought$hhi_avg   <- hhi_drought$hhi
hhi_drought$month_max <- hhi_drought$month
hhi_drought$year_max  <- hhi_drought$year
hhi_drought$ts_max  <- hhi_drought$ts
hhi_drought$ts_start  <- hhi_drought$ts
hhi_drought$ts_stop   <- hhi_drought$ts
hhi_drought$duration  <- 1
hhi_drought$maximum   <- FALSE

n <- 1
for(i in 2:nrow(hhi_drought)) {
      ## check if prevoius month
      if((hhi_drought$ts_start[i]-hhi_drought$ts_stop[i-1]) < 0.1) {
        hhi_drought$n[i] = n
        hhi_drought$n[i-1] = n
        if(hhi_drought$hhi.fmax[i] < hhi_drought$hhi.fmax[i-1]) {
          hhi_drought$month_max[i-1] <- hhi_drought$month_max[i]
          hhi_drought$year_max[i-1] <- hhi_drought$year_max[i]   
          hhi_drought$ts_max[i-1] <- hhi_drought$ts_max[i]             
          hhi_drought$maximum[i] <- TRUE
          hhi_drought$maximum[i-1] <- FALSE          
        } else {
          hhi_drought$month_max[i] <- hhi_drought$month_max[i-1]
          hhi_drought$year_max[i] <- hhi_drought$year_max[i-1]
          hhi_drought$ts_max[i] <- hhi_drought$ts_max[i-1]          
          #hhi_drought$maximum[i] <- FALSE
          #hhi_drought$maximum[i-1] <- TRUE           
        }
        hhi_drought$hhi.fmax[i] <- min(hhi_drought$hhi.fmax[i],hhi_drought$hhi.fmax[i-1])
        hhi_drought$hhi_max[i] <- min(hhi_drought$hhi_max[i],hhi_drought$hhi_max[i-1])
        hhi_drought$hhi_sum[i] <- (hhi_drought$hhi_sum[i] + hhi_drought$hhi_sum[i-1])
        hhi_drought$duration[i] <- (hhi_drought$duration[i] + hhi_drought$duration[i-1])
        hhi_drought$ts_start[i] <- min(hhi_drought$ts_start[i],hhi_drought$ts_start[i-1])
        hhi_drought$ts_stop[i]  <- max(hhi_drought$ts_stop[i],hhi_drought$ts_stop[i-1])
      } else {
        n <- n+1
    }
}

for(i in nrow(hhi_drought):2) {
  if(hhi_drought$n[i] == hhi_drought$n[i-1]) {
    hhi_drought$hhi.fmax[i-1]  <- min(hhi_drought$hhi.fmax[i],hhi_drought$hhi.fmax[i-1])
    hhi_drought$hhi_max[i-1]   <- min(hhi_drought$hhi_max[i],hhi_drought$hhi_max[i-1])
    hhi_drought$month_max[i-1] <- hhi_drought$month_max[i]
    hhi_drought$year_max[i-1]  <- hhi_drought$year_max[i]
    hhi_drought$ts_max[i-1]  <- hhi_drought$ts_max[i]    
    hhi_drought$duration[i-1]  <- hhi_drought$duration[i]
    hhi_drought$ts_start[i-1]  <- hhi_drought$ts_start[i]
    hhi_drought$ts_stop[i-1]   <- hhi_drought$ts_stop[i]    
    hhi_drought$hhi_sum[i-1]   <- hhi_drought$hhi_sum[i]
    if(hhi_drought$hhi.f[i-1] > hhi_drought$hhi.fmax[i]) {
       hhi_drought$maximum[i-1] <- FALSE
    }
  }
}

hhi_drought$hhi_avg <- hhi_drought$hhi_sum / hhi_drought$duration

hhi_periods <- subset(hhi_drought, hhi_drought$maximum)
hhi_periods <- hhi_periods[order(hhi_periods$hhi_avg),]

write.table(hhi_periods, file = "droughts2.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "escape", fileEncoding = "UTF-8")

data <- hhi_periods

# Most basic bubble plot
ggplot(data=hhi_periods, aes(x=duration, y=-hhi_avg, size=-hhi_max, color=month_max)) +
  geom_point(alpha=0.5) +
  scale_x_continuous(trans='log') +
  scale_size(range = c(1, 12), name="HHI max")

ggplot(data=hhi_periods, aes(x=month_max, y=duration, size=-hhi_avg, color=hhi_max, label=year_max)) +
  geom_text(alpha=0.5)+
  scale_size(range = c(1, 12), name="HHI avg")

library("RColorBrewer")
monthColors = brewer.pal(n = 12, name = "Paired")

ggplot(data=hhi_periods, aes(x=-hhi.fmax, y=-hhi_avg, size=duration, color=month_max, label=year_max)) +
  geom_text(alpha=0.8, check_overlap = FALSE)+
  scale_size(range = c(2.5, 6), name="max", trans="log")+
  scale_color_gradientn(colors=monthColors) 

hhiColors = brewer.pal(n = 12, name = "YlOrRd")
ggplot(data=hhi_periods, aes(y=-hhi.fmax, x=month_max, size=-hhi_avg, color=duration, label=year_max)) +
  geom_text(alpha=0.8, check_overlap = FALSE)+
  scale_size(range = c(2.5, 8), name="HHI avg")+
  scale_color_gradientn(colors=hhiColors) 

hhiColors = brewer.pal(n = 12, name = "YlOrRd")
ggplot(data=hhi_periods, aes(y=-hhi_avg, x=year_max, size=duration, color=-hhi.fmax, label=year_max)) +
  geom_text(alpha=0.8, check_overlap = FALSE)+
  scale_size(range = c(5, 12), name="Duration", trans="log")+
  scale_color_gradientn(colors=hhiColors, limits=c(0.25,4.5))

droughtColors = brewer.pal(n = 5, name = "YlOrRd")

ggplot(data=hhi_periods, aes(y=-hhi.fmax, x=year, size=duration, color=-hhi.avg, label=year)) +
  theme_classic(base_size=80) +
  theme( legend.key.width = unit(2,"cm"), legend.key.height = unit(4,"cm")) +
  guides(fill=guide_legend(title="Droughts", reverse = TRUE)) +
  xlab("Year") + ylab("HHI max") +
  geom_text(alpha=0.8, check_overlap = FALSE)+
  scale_size(range = c(6, 24), name="Duration", trans="log", breaks=c(1,2,5,10,20))+
  scale_color_gradientn(colors=droughtColors, limits=c(0,4), name="HHI Ø", breaks=c(0,1,2,3,4))

monthColors = c("blue","green", "red","brown", "blue")
ggplot(data=hhi_periods, aes(y=-hhi.fmax, x=year_max, size=duration, color=month_max, label=year_max)) +
  geom_text(alpha=0.8, check_overlap = FALSE)+
  scale_size(range = c(5, 12), name="Duration", trans="log")+
  scale_color_gradientn(colors=monthColors)


yearColors = brewer.pal(n = 9, name = "Spectral")
ggplot(hhi_drought, aes(group=n, color=year_max, y=-hhi.f, x=12*(ts-ts_max)))+
  geom_line()+
  scale_x_continuous(limits=c(-18,6)) +  
  scale_color_gradientn(colors=yearColors)

ggplot(hhi_drought, aes(group=n, color=year_max, y=-hhi.f, x=month_max+12*(ts-ts_max)))+
  geom_line()+
  scale_x_continuous(limits=c(-18,18)) +  
  scale_color_gradientn(colors=yearColors)



ggplot(data=hhi_periods, aes(y=-hhi_avg, x=duration, size=-hhi.fmax, color=year_max, label=year_max)) +
  geom_text(alpha=0.8, check_overlap = FALSE)+
  scale_size(range = c(5, 12), name="HHI max")+
  scale_color_gradientn(colors=hhiColors)


droughtColors = brewer.pal(n = 5, name = "YlOrRd")

mp <- ggplot(hhi_drought, aes(year_max, round(12*(ts-ts_max))))
mp + geom_raster(aes(fill=-hhi))+
  #theme_classic(base_size=80) +
  theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(-9,-6,-3,0,3,6,9), limits=c(-12,12))+
  scale_x_continuous(limits=c(1520,1560)) +  
  scale_fill_gradientn(colors=droughtColors, limits=c(0,4)) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

mp <- ggplot(hhi_drought, aes(year_max, month_max+round(12*(ts-ts_max))))
mp + geom_raster(aes(fill=-hhi))+
  #theme_classic(base_size=80) +
  theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(3,6,9,12), limits=c(-12,24))+
  scale_x_continuous(limits=c(1520,1560)) +  
  scale_fill_gradientn(colors=droughtColors) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))

mp <- ggplot(hhi_drought, aes(year, month))
mp + geom_raster(aes(fill=-hhi))+
  #theme_classic(base_size=80) +
  theme_classic() +
  labs(x="Year", y="Month", title="", subtitle="") +
  scale_y_continuous(breaks=c(3,6,9,12), limits=c(1,12))+
  scale_x_continuous(limits=c(1520,1560)) +  
  scale_fill_gradientn(colors=droughtColors) + 
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="HHI", reverse = TRUE))
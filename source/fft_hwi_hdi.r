#install.packages("ggplot2")
#install.packages("RColorBrewer")

library("RColorBrewer")
require("ggplot2")

par(mfrow=c(1,1))

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


setwd("C:/Code/climData/glaser2019")

hhi <- read.csv("csv/hhi_1500_2019.csv", sep=",", na = "NA")


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

hhi$hhi5 <- frqFilter(hhi$hhi, years=5.0)
hhi$hdi5 <- frqFilter(hhi$hdi, years=5.0)
hhi$hwi5 <- frqFilter(hhi$hwi, years=5.0)
hhi$hdi4 <- frqFilter(hhi$hdi, years=4.0)
hhi$hwi4 <- frqFilter(hhi$hwi, years=4.0)
hhi$hdi3 <- frqFilter(hhi$hdi, years=3.0)
hhi$hwi3 <- frqFilter(hhi$hwi, years=3.0)
hhi$hdi2 <- frqFilter(hhi$hdi, years=2.0)
hhi$hwi2 <- frqFilter(hhi$hwi, years=2.0)
hhi$hdi1 <- frqFilter(hhi$hdi, years=1.0)
hhi$hwi1 <- frqFilter(hhi$hwi, years=1.0)
hhi$hhi1 <- frqFilter(hhi$hhi, years=1.0)

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

hhiColors = brewer.pal(n = 9, name = "Spectral")
hhiColors[5]

mixWColor <- function(t,p) {
  c <- mixColor(t,t, 
                cxmin=hhiColors[5], cxmid=hhiColors[7], cxmax=hhiColors[9],
                cymin=hhiColors[5], cymid=hhiColors[7], cymax=hhiColors[9]                
  )
}
mixDColor <- function(t,p) {
  c <- mixColor(t,t, 
                cxmin=hhiColors[1], cxmid=hhiColors[3], cxmax=hhiColors[5],
                cymin=hhiColors[1], cymid=hhiColors[3], cymax=hhiColors[5]                
  )
}

mixHColor <- function(t,p) {
  c <- mixColor(t,t, 
                cxmin=hhiColors[1], cxmid=hhiColors[5], cxmax=hhiColors[9],  
                cymin=hhiColors[1], cymid=hhiColors[5], cymax=hhiColors[9]                
  )
  return(c)
}

mp <- ggplot(hhi, aes())
mp + 
  
  geom_raster(aes(year, 5), fill=mixWColor(1*hhi$hwi1, 1*hhi$hwi1))+  
  geom_raster(aes(year, 4), fill=mixWColor(1.5*hhi$hwi2, 1.5*hhi$hwi2))+   
  geom_raster(aes(year, 3), fill=mixWColor(2*hhi$hwi3, 2*hhi$hwi3))+  
  geom_raster(aes(year, 2), fill=mixWColor(2.5*hhi$hwi4, 2.5*hhi$hwi4))+  
  geom_raster(aes(year, 1), fill=mixWColor(3*hhi$hwi5, 3*hhi$hwi5))+
  geom_raster(aes(year, 0), fill=mixHColor(3*hhi$hhi5, 3*hhi$hhi5))+
  geom_raster(aes(year,-1), fill=mixDColor(3*hhi$hdi5, 3*hhi$hdi5))+  
  geom_raster(aes(year,-2), fill=mixDColor(2.5*hhi$hdi4, 2.5*hhi$hdi4))+    
  geom_raster(aes(year,-3), fill=mixDColor(2*hhi$hdi3, 2*hhi$hdi3))+  
  geom_raster(aes(year,-4), fill=mixDColor(1.5*hhi$hdi2, 1.5*hhi$hdi2))+    
  geom_raster(aes(year,-5), fill=mixDColor(1*hhi$hdi1, 1*hhi$hdi1))+  

  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), lab=c("1y D","2y D","3y D","4y D","5y D","5y H","5y W", "4y W", "3y W", "2y W", "1y W"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="TI/PI", reverse = TRUE)) +
  geom_hline(aes(yintercept = 3+0)) +
  geom_line(aes(y=3+1.5*hwi5, x=time, color="Filtered"), size=2) +
  geom_hline(aes(yintercept = 0+0)) +
  geom_line(aes(y=0+1.0*hhi5, x=time, color="Filtered"), size=2) +
  geom_hline(aes(yintercept = -3+0)) +
  geom_line(aes(y=-3+1.5*hdi5, x=time, color="Filtered"), size=2) +
  scale_color_manual("Filtered", values=c("#000000"), labels=c("5y"))    
#geom_line(aes(y=10+0.5*pt1$hhim, x=pt1$time))   



mp <- ggplot(hhi, aes())
mp + 
  
  geom_raster(aes(year, 1), fill=mixWColor(1*hhi$hwi1, 1*hhi$hwi1))+  
  geom_raster(aes(year, 2), fill=mixWColor(1.5*hhi$hwi2, 1.5*hhi$hwi2))+   
  geom_raster(aes(year, 3), fill=mixWColor(2*hhi$hwi3, 2*hhi$hwi3))+  
  geom_raster(aes(year, 4), fill=mixWColor(2.5*hhi$hwi4, 2.5*hhi$hwi4))+  
  geom_raster(aes(year, 5), fill=mixWColor(3*hhi$hwi5, 3*hhi$hwi5))+
  geom_raster(aes(year, 0), fill=mixHColor(1*hhi$hhi1, 1*hhi$hhi1))+
  geom_raster(aes(year,-5), fill=mixDColor(3*hhi$hdi5, 3*hhi$hdi5))+  
  geom_raster(aes(year,-4), fill=mixDColor(2.5*hhi$hdi4, 2.5*hhi$hdi4))+    
  geom_raster(aes(year,-3), fill=mixDColor(2*hhi$hdi3, 2*hhi$hdi3))+  
  geom_raster(aes(year,-2), fill=mixDColor(1.5*hhi$hdi2, 1.5*hhi$hdi2))+    
  geom_raster(aes(year,-1), fill=mixDColor(1*hhi$hdi1, 1*hhi$hdi1))+  
  
  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(-5,-4,-3,-2,-1,0,1,2,3,4,5), lab=c("5y D","4y D","3y D","2y D","1y D","1y H","1y W", "2y W", "3y W", "4y W", "5y W"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="TI/PI", reverse = TRUE)) +
  geom_hline(aes(yintercept = 3+0)) +
  geom_line(aes(y=3+1.5*hwi5, x=time, color="Filtered"), size=2) +
  geom_hline(aes(yintercept = 0+0)) +
  geom_line(aes(y=0+1.0*hhi5, x=time, color="Filtered"), size=2) +
  geom_hline(aes(yintercept = -3+0)) +
  geom_line(aes(y=-3+1.5*hdi5, x=time, color="Filtered"), size=2) +
  scale_color_manual("Filtered", values=c("#000000"), labels=c("5y"))    



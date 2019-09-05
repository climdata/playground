library("RColorBrewer")
require("ggplot2")

setwd("C:/Code/climData/glaser2019")
spi <- read.csv("csv/spi_1500_2019.csv", sep=",", na = "NA")

precColors = c("#AA6010", "#FCF0C2","#23AB30")

mp <- ggplot(spi, aes())
mp + 
  
  geom_raster(aes(year,12), fill=spi$spi12)+  
  geom_raster(aes(year,11), fill=spi$spi11)+   
  geom_raster(aes(year,10), fill=spi$spi10)+  
  geom_raster(aes(year, 9), fill=spi$spi9)+  
  geom_raster(aes(year, 8), fill=spi$spi8)+
  geom_raster(aes(year, 7), fill=spi$spi7)+
  geom_raster(aes(year, 6), fill=spi$spi6)+  
  geom_raster(aes(year, 5), fill=spi$spi5)+    
  geom_raster(aes(year, 4), fill=spi$spi4)+  
  geom_raster(aes(year, 3), fill=spi$spi3)+    
  geom_raster(aes(year, 2), fill=spi$spi2)+  
  geom_raster(aes(year, 1), fill=spi$spi1)+ 
  
  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(12,11,10,9,8,7,6,5,4,3,2,1), lab=c("SPI12","SPI11","SPI10","SPI9","SPI8","SPI7","SPI6","SPI5","SPI4","SPI3","SPI2","SPI1"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="SPI", reverse = TRUE)) +
  scale_fill_gradientn(colors=precColors) 
#+    
#geom_line(aes(y=10+0.5*pt1$hhim, x=pt1$time))   

mp <- ggplot(spi, aes())
mp +
  geom_raster(aes(year,12, fill=spi$spi12))+
  geom_raster(aes(year,11, fill=spi$spi11))+   
  geom_raster(aes(year,10, fill=spi$spi10))+  
  geom_raster(aes(year, 9, fill=spi$spi9))+  
  geom_raster(aes(year, 8, fill=spi$spi8))+
  geom_raster(aes(year, 7, fill=spi$spi7))+
  geom_raster(aes(year, 6, fill=spi$spi6))+  
  geom_raster(aes(year, 5, fill=spi$spi5))+    
  geom_raster(aes(year, 4, fill=spi$spi4))+  
  geom_raster(aes(year, 3, fill=spi$spi3))+    
  geom_raster(aes(year, 2, fill=spi$spi2))+  
  geom_raster(aes(year, 1, fill=spi$spi1))+ 
  #theme_classic() +
  theme_classic(base_size=80) +
  scale_y_continuous(breaks=c(12,11,10,9,8,7,6,5,4,3,2,1), lab=c("SPI12","SPI11","SPI10","SPI9","SPI8","SPI7","SPI6","SPI5","SPI4","SPI3","SPI2","SPI1"))+
  scale_x_continuous(limits=c(1500,2020)) +  
  #scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  scale_fill_gradientn(colors=precColors) +  
  theme( legend.key.width = unit(2,"cm")) +
  guides(fill=guide_legend(title="SPI", reverse = TRUE)) 

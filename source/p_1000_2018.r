#install.packages("ggplot2")


require("ggplot2")
par(mfrow=c(1,1))

custom.col <- c("#FFDB6D", "#C4961A", "#F4EDCA", 
                "#D16103", "#C3D7A4", "#52854C", "#4E84C4", "#293352")


setwd("C:/Code/climData/glaser2019")

pt <- read.csv("csv/p_1000_2018.csv", sep=",", na = "NA")

pt2 <- subset(pt, !is.na(pt$precipitation))


pt2$m <- NULL
mp <- ggplot(pt2, aes(year, month))
mp + geom_raster(aes(fill=precipitation))+
  theme_classic() +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_x_continuous(limits=c(1000,2020)) +  
  theme(panel.background = element_rect(fill = '#FFFFFF', colour = 'white'), legend.position="right", text=element_text(size=14, family="Calibri"))+
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30")

#+
#  geom_rect(xmin=d$x1, xmax=d$x2, ymin=d$y1, ymax=d$y2, color="black", alpha=0.5, fill=t) +
#  geom_text(data=d, aes(x=xl, y=yl, label=r), size=4) 


#d=data.frame(x1=c(1000,1500), x2=c(2000,2000), y1=c(6,0), y2=c(9,12), 
#             xl=c(1050,1600), yl=c(8,11),
#             t=c('summer: 1000y','12 month: 500y'), r=c(1,2))

mp <- ggplot(pt2, aes(year, month))
mp + geom_tile(aes(fill=precipitation))+
  theme_classic() +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_x_continuous(limits=c(1000,2020)) +  
  theme(panel.background = element_rect(fill = '#FFFFFF', colour = 'white'), legend.position="right", text=element_text(size=14, family="Calibri"))+
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  geom_rect(data=d, mapping=aes(xmin=d$x1, xmax=d$x2, ymin=d$y1, ymax=d$y2, color="black", alpha=0.5), fill=t) +
  geom_text(data=d, aes(x=xl, y=yl, label=r), size=4) 



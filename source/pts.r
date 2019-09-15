library("ggplot2")

t0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/ti_1500_2xxx_monthly.csv", sep=",", na = "NA")
t1 <- t0[,c("year","month","ti")]
p0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/pi_1500_2xxx_monthly.csv", sep=",", na = "NA")
spifull <- read.csv("https://raw.githubusercontent.com/climdata/dwdSPI/master/csv/spi_de.csv", sep=",", na = "NA")
spi <- spifull[, c("year","month","spi1")]

tps <- merge(t1,p0, by=c("year","month"))
tps <- merge(tps,spi, by=c("year","month"))
tps$t1 <- sin(tps$month*pi/6)
tps$t2 <- cos(tps$month*pi/6)

tps <- tps[order(tps$ts),]

mx2 <- lm(spi1 ~ (pi+ti+t1+t2)^2, tps)
summary(mx2)
pr2 <- predict(mx2, newdata=tps, se.fit=TRUE) 
tps$spi2 <- pr2$fit
tps$se2 <- pr2$se

mx3 <- lm(spi1 ~ pi, tps)
summary(mx3)
pr3 <- predict(mx3, newdata=tps, se.fit=TRUE) 
tps$spi3 <- pr3$fit
tps$se3 <- pr3$se

mx4 <- lm(spi1 ~ pi+ti*t2, tps)
summary(mx4)
pr4 <- predict(mx4, newdata=tps, se.fit=TRUE) 
tps$spi4 <- pr4$fit
tps$se4 <- pr4$se

p <- ggplot(data = tps, aes(x = spi1, y = spi2)) +
   geom_point(aes(x = spi1, y = spi3), color="#00FF00", alpha=0.3, size=2) +
   geom_point(aes(x = spi1, y = spi4), color="#0000BB", alpha=0.3, size=2) +
   geom_point(aes(x = spi1, y = spi2), color="#FF0000", alpha=0.5, size=2)
p
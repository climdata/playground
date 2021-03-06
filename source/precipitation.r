library("ggplot2")
library("discSurv")
library("pec")

t0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/ti_1500_2xxx_monthly.csv", sep=",", na = "NA")
t1 <- t0[,c("year","month","ti")]
p0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/pi_1500_2xxx_monthly.csv", sep=",", na = "NA")
prec0 <- read.csv("https://raw.githubusercontent.com/climdata/dwdPrecipitation/master/csv/monthly_precipitation_de.csv", sep=",", na = "NA")
prec1 <- prec0[, c("year","month","Deutschland")]

tps <- merge(t1,p0, by=c("year","month"))
tps <- merge(tps,prec1, by=c("year","month"))
tps$t1 <- sin((tps$month-1)*pi/6)
tps$t2 <- cos((tps$month-1)*pi/6)
tps <- tps[order(tps$ts),]

mx5 <- lm(Deutschland ~ (pi+ti)*(t1+t2), tps)
summary(mx5)
pr5 <- predict(mx5, newdata=tps, se.fit=TRUE) 
tps$spi5 <- pr5$fit
tps$se5 <- pr5$se

mx2 <- lm(Deutschland ~ pi+ti+t1+t2, tps)
summary(mx2)
pr2 <- predict(mx2, newdata=tps, se.fit=TRUE) 
tps$spi2 <- pr2$fit
tps$se2 <- pr2$se

tps$pi3 <- (tps$pi+3)/60
tps$ti3 <- (tps$ti+3)/60
tps$t13 <- (tps$t1+1)/20
tps$t23 <- (tps$t2+1)/20
tps$Deutschland3 <- (tps$Deutschland)/200

mx3 <- lm(Deutschland ~ (pi+ti+t1+t2)^2, tps)
#mx3 <- glm(formula=Deutschland3 ~ (pi+ti+t1+t2)^2, data=tps, family=binomial(link=gumbel()))

summary(mx3)
pr3 <- predict(mx3, newdata=tps, se.fit=TRUE) 
tps$spi3 <- pr3$fit 
tps$se3 <- pr3$se

mx4 <- lm(Deutschland ~ t1+t2, tps)
summary(mx4)
pr4 <- predict(mx4, newdata=tps, se.fit=TRUE) 
tps$spi4 <- pr4$fit
tps$se4 <- pr4$se

p <- ggplot(data = tps, aes(x = Deutschland, y = spi3)) +
  #geom_point(aes(y = spi4), color="#00AA00", alpha=0.2, size=2) +
  geom_point(aes(y = spi3), color="#0000BB", alpha=0.3, size=2) +
  geom_point(aes(y = spi2), color="#FF0000", alpha=0.4, size=2) +
  geom_point(aes(y = spi5), color="#00FF00", alpha=0.5, size=2) +
  geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) 
p


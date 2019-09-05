library(zoo)
library("ggplot2")
library(gridExtra)
library(grid)
library(qdapTools)

setwd("C:/Code/climData/glaser2019")

hhi <- read.csv("csv/hhi_summer_1000_2019.csv", sep=",", na = "NA")

half  <- read.csv("csv/tmb_folgen_5_10.csv", sep=",")
quart <- read.csv("csv/tmb_folgen_6_8.csv", sep=",")

both2 <- merge(hhi,half, by=c("year"))
both4 <- merge(hhi,quart, by=c("year"))

new2 <- subset(both2, both2$year<1996 & both2$year>1499)
old2 <- subset(both2, both2$year<1500 & both2$year>999)

old2 <- subset(old2, old2$hhi != 0.0)
new2 <- subset(new2, new2$hhi != 0.0)

old2 <- subset(old2, old2$count > 0.0)
new2 <- subset(new2, new2$count > 0.0)

new4 <- subset(both4, both4$year<1996 & both4$year>1499)
old4 <- subset(both4, both4$year<1500 & both4$year>999)

old4 <- subset(old4, old4$hhi != 0.0)
new4 <- subset(new4, new4$hhi != 0.0)

old4 <- subset(old4, old4$count > 0.0)
new4 <- subset(new4, new4$count > 0.0)


df <- data.frame(y = new4[,"hhi"], x = new4[,"davg"])
df <- subset(df, !is.na(df$x))
mx <- lm(y ~ x, df);  
slope <- unname(coef(mx)[2])
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(mx)[1]), digits = 3),
                      b = format(unname(coef(mx)[2]), digits = 5),
                      r2 = format(summary(mx)$r.squared, digits = 3)))
eq <- as.character(as.expression(eq))

p <- ggplot(data = df, aes(x = x, y = y)) +
  theme_classic(base_size=60) +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-3.5,3.5)) +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=0)) +
  labs(x="tmb impact", y="HHI", title="", subtitle="") +
  geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) +
  geom_point(color="#0000AA", alpha=0.3, size=5) +
  geom_text(x = 2, y = -4, label = eq, parse = TRUE, size=15)

p

df <- data.frame(y = old4[,"hhi"], x = old4[,"davg"])
df <- subset(df, !is.na(df$x))
mx <- lm(y ~ x, df);  
slope <- unname(coef(mx)[2])
eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(mx)[1]), digits = 3),
                      b = format(unname(coef(mx)[2]), digits = 5),
                      r2 = format(summary(mx)$r.squared, digits = 3)))
eq <- as.character(as.expression(eq))

p <- ggplot(data = df, aes(x = x, y = y)) +
  theme_classic(base_size=60) +
  scale_x_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-3.5,3.5)) +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3)) +
  geom_hline(aes(yintercept=0)) +
  geom_vline(aes(xintercept=0)) +
  labs(x="tmb impact", y="HHI", title="", subtitle="") +
  geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) +
  geom_point(color="#0000AA", alpha=0.3, size=5) +
  geom_text(x = 2, y = -2, label = eq, parse = TRUE, size=15)

p
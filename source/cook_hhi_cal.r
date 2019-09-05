library(zoo)
library("ggplot2")
library(gridExtra)
library(grid)
library(qdapTools)

setwd("C:/Code/climData/glaser2019")

hhi <- read.csv("csv/hhi_summer_1000_2019.csv", sep=",", na = "NA")

cook <- read.csv("https://raw.githubusercontent.com/climdata/cook2015/master/csv/cook_de.csv", sep=",")

both <- merge(hhi,cook, by=c("year"))

new <- subset(both, both$year<1996 & both$year>1499)
old <- subset(both, both$year<1500 & both$year>999)

old <- subset(old, old$hhi != 0.0)
new <- subset(new, new$hhi != 0.0)

old <- subset(old, old$hhi*old$DE > 0.0)
new <- subset(new, new$hhi*new$DE > 0.0)


df <- data.frame(y = new[,"hhi"], x = new[,"DE"])
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
  labs(x="cook", y="HHI", title="", subtitle="") +
  geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) +
  geom_point(color="#0000AA", alpha=0.3, size=5) +
  geom_text(x = 2, y = -4, label = eq, parse = TRUE, size=15)

p

df <- data.frame(y = old[,"hhi"], x = old[,"DE"])
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
  labs(x="cook", y="HHI", title="", subtitle="") +
  geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) +
  geom_point(color="#0000AA", alpha=0.3, size=5) +
  geom_text(x = 2, y = -2, label = eq, parse = TRUE, size=15)

p
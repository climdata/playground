library(zoo)
library("ggplot2")
library(gridExtra)
library(grid)
library(qdapTools)

setwd("C:/Code/climData/glaser2019")

piAll <- read.csv("csv/p_1000_2018.csv", sep=",", na = "NA")

hiNew <- read.csv("csv/hhi_1500_2019.csv", sep=",", na = "NA")
hiNew <- subset(hiFull, hiFull$year<1996)

## Calculate summer average 

hiSummer <- rollapply(hiNew$hhi, width=3, by=1, FUN=sum)
hiNew$hiSummer <- hiNew$hhi
hiNew$hiSummer[3:length(hiNew$hhi)] <- hiSummer/3.0
hiNewSummer <- subset(hiNew, hiNew$month==8)
hiNewSummer <- hiNewSummer[,c('year','month','time', 'hiSummer')]

piSummer <- rollapply(piAll$precipitation, width=3, by=1, FUN=sum)
piAll$piSummer <- piAll$precipitation
piAll$piSummer[3:length(piAll$precipitation)] <- piSummer/3.0
piAllSummer <- subset(piAll, piAll$month==8)

piNewSummer <- subset(piAllSummer, piAllSummer$year>1499 & piAllSummer$year<1996)
piNewSummer <- piNewSummer[,c('year','month', 'piSummer')]

## calibrate hhi vs average summper pi

hipi <- merge(hiNewSummer,piAllSummer, by=c("year","month"))

df <- data.frame(y = hipi[,"hiSummer"], x = hipi[,"piSummer"])
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
  labs(x="PI", y="HHI", title="", subtitle="") +
  geom_smooth(method = "lm", se=TRUE, color="cyan", formula = y ~ x) +
  geom_point(color="#0000AA", alpha=0.3, size=5) +
  geom_text(x = 2, y = -4, label = eq, parse = TRUE, size=15)


## Calculate 1000 years

piAllSummer$hhi <- piAllSummer$piSummer
piAllSummer$hdi <- piAllSummer$piSummer
piAllSummer$hwi <- piAllSummer$piSummer

for(i in 1:nrow(piAllSummer)) {
  piAllSummer$hhi[i] <- slope*piAllSummer$piSummer[i]
  if(is.na(piAllSummer$hhi[i])) {
    piAllSummer$hhi[i] <- 0.0
  }
  if(piAllSummer$year[i] < 1500) {
    #from cook comparison
    #piAllSummer$hhi[i] <- 0.6225*piAllSummer$hhi[i]
  }
  piAllSummer$hdi[i] <- min(0.0, piAllSummer$hhi[i])
  piAllSummer$hwi[i] <- max(0.0, piAllSummer$hhi[i])
}

hiNewSummer$hdi <- hiNewSummer$hiSummer
for(i in 1:nrow(hiNewSummer)) {
  hiNewSummer$hdi[i] <- min(0.0, hiNewSummer$hiSummer[i])
}

mp1 <- ggplot() +
  theme_classic(base_size=80) +
  #coord_cartesian(ylim=c(-4,4)) +
  scale_y_continuous(breaks=c(-3,-2,-1,0,1,2,3), limits=c(-5,5)) +
  labs(x="Year", y="HDI", title="", subtitle="") +
  geom_hline(aes(yintercept=0)) +
  geom_line(aes(y=-hiNewSummer$hdi, x=hiNewSummer$year, color="a:HDI"), size=2) +
  geom_line(aes(y=piAllSummer$hdi, x=piAllSummer$year, color="b:HDI"), size=2) +
  scale_color_manual(
    name = "", 
    labels = c("HDI monthly", "HDI summer"),
    breaks  = c("a:HDI", "b:HDI"),
    values = c("a:HDI"="#33ccff", "b:HDI"="#ff9933")
  ) + 
  theme( legend.key.width = unit(2,"cm")) 
mp1
  
  
hhi <- piAllSummer[,c('year', 'hdi', 'hwi', 'hhi')]
hhi$hdi <- round(hhi$hdi, digits=6)
hhi$hwi <- round(hhi$hwi, digits=6)
hhi$hhi <- round(hhi$hhi, digits=6)

write.table(hhi, file = "csv/hhi_summer_1000_2019.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = "escape", fileEncoding = "UTF-8")



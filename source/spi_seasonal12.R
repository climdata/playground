require("ggplot2")

spi_all <- read.csv("https://raw.githubusercontent.com/climdata/drought2019/master/csv/spi_1500_2xxx.csv", sep=",")

spi.jan <- subset(spi_all, spi_all$month == 1)   # W w
spi.feb <- subset(spi_all, spi_all$month == 2)   # W w
spi.mar <- subset(spi_all, spi_all$month == 3)   # w f
spi.apr <- subset(spi_all, spi_all$month == 4)   # W f
spi.may <- subset(spi_all, spi_all$month == 5)   # S f
spi.jun <- subset(spi_all, spi_all$month == 6)   # S s
spi.jul <- subset(spi_all, spi_all$month == 7)   # S s
spi.aug <- subset(spi_all, spi_all$month == 8)   # S s
spi.sep <- subset(spi_all, spi_all$month == 9)   # S h
spi.oct <- subset(spi_all, spi_all$month == 10)  # S h
spi.nov <- subset(spi_all, spi_all$month == 11)  # W h
spi.dec <- subset(spi_all, spi_all$month == 12)  # W w


mp <- ggplot()
mp + geom_line(aes(y=spi.jan$spi1, x=spi.jan$year), color="blue") +
  geom_line(aes(y=spi.feb$spi1, x=spi.feb$year), color="blue") +
  geom_line(aes(y=spi.mar$spi1, x=spi.mar$year), color="green") +
  geom_line(aes(y=spi.apr$spi1, x=spi.apr$year), color="green") +
  geom_line(aes(y=spi.may$spi1, x=spi.may$year), color="green") +  
  geom_line(aes(y=spi.jun$spi1, x=spi.jun$year), color="red") +  
  geom_line(aes(y=spi.jul$spi1, x=spi.jul$year), color="red") +
  geom_line(aes(y=spi.aug$spi1, x=spi.aug$year), color="red")  
  geom_line(aes(y=spi.sep$spi1, x=spi.sep$year), color="brown")  
  geom_line(aes(y=spi.oct$spi1, x=spi.oct$year), color="brown")
  geom_line(aes(y=spi.nov$spi1, x=spi.nov$year), color="brown")
  geom_line(aes(y=spi.dec$spi1, x=spi.dec$year), color="blue")


do_fft <- function(pic, filterYears) {
  len <- length(pic)
  ## mirror pic
  pic <- append(pic, pic)
  for(i in 1:len) {
    pic[i+len] <- pic[1+len-i]
  }
  frq <- fft(pic, inverse = FALSE)
  frq1 <- frq
  start = round(len/(1*filterYears)) ## yearly
  stop  = round(2*len-start)
  frq1[start:stop] <- 0.0 
  pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
  pic1 <- pic1[1:len]
  return(pic1)
}  
  

filterYears = 5.0   #filter 5y
#filterYears = 58
#FFT for HDI
spi.jan$spi1.5y <- do_fft(spi.jan$spi1,filterYears)
spi.feb$spi1.5y <- do_fft(spi.feb$spi1,filterYears)
spi.mar$spi1.5y <- do_fft(spi.mar$spi1,filterYears)
spi.apr$spi1.5y <- do_fft(spi.apr$spi1,filterYears)
spi.may$spi1.5y <- do_fft(spi.may$spi1,filterYears)
spi.jun$spi1.5y <- do_fft(spi.jun$spi1,filterYears)
spi.jul$spi1.5y <- do_fft(spi.jul$spi1,filterYears)
spi.aug$spi1.5y <- do_fft(spi.aug$spi1,filterYears)
spi.sep$spi1.5y <- do_fft(spi.sep$spi1,filterYears)
spi.oct$spi1.5y <- do_fft(spi.oct$spi1,filterYears)
spi.nov$spi1.5y <- do_fft(spi.nov$spi1,filterYears)
spi.dec$spi1.5y <- do_fft(spi.dec$spi1,filterYears)


mp <- ggplot()
mp + geom_line(aes(y=spi.jan$spi1.5y, x=spi.jan$year), color="blue", lty=1) +
  geom_line(aes(y=spi.feb$spi1.5y, x=spi.feb$year), color="blue", lty=2) + 
  geom_line(aes(y=spi.mar$spi1.5y, x=spi.mar$year), color="green", lty=4) +  
  geom_line(aes(y=spi.apr$spi1.5y, x=spi.apr$year), color="green", lty=1) +
  geom_line(aes(y=spi.may$spi1.5y, x=spi.may$year), color="green", lty=2) +
  geom_line(aes(y=spi.jun$spi1.5y, x=spi.jun$year), color="orange", lty=4) +  
  geom_line(aes(y=spi.jul$spi1.5y, x=spi.jul$year), color="orange", lty=1) +
  geom_line(aes(y=spi.aug$spi1.5y, x=spi.aug$year), color="orange", lty=2) +
  geom_line(aes(y=spi.sep$spi1.5y, x=spi.sep$year), color="brown", lty=4) +  
  geom_line(aes(y=spi.oct$spi1.5y, x=spi.oct$year), color="brown", lty=1) +
  geom_line(aes(y=spi.nov$spi1.5y, x=spi.nov$year), color="brown", lty=2) +
  geom_line(aes(y=spi.dec$spi1.5y, x=spi.dec$year), color="blue", lty=4) 

spi1y.jan <- spi.jan
spi5y.jan <- spi.jan
spi1y.feb <- spi.feb
spi5y.feb <- spi.feb
spi1y.mar <- spi.mar
spi5y.mar <- spi.mar
spi1y.apr <- spi.apr
spi5y.apr <- spi.apr
spi1y.may <- spi.may
spi5y.may <- spi.may
spi1y.jun <- spi.jun
spi5y.jun <- spi.jun
spi1y.jul <- spi.jul
spi5y.jul <- spi.jul
spi1y.aug <- spi.aug
spi5y.aug <- spi.aug
spi1y.sep <- spi.sep
spi5y.sep <- spi.sep
spi1y.oct <- spi.oct
spi5y.oct <- spi.oct
spi1y.nov <- spi.nov
spi5y.nov <- spi.nov
spi1y.dec <- spi.dec
spi5y.dec <- spi.dec



names(spi1y.jan)[names(spi1y.jan) == "spi1"] <- "jan"
names(spi5y.jan)[names(spi5y.jan) == "spi1.5y"] <- "jan"
names(spi1y.feb)[names(spi1y.feb) == "spi1"] <- "feb"
names(spi5y.feb)[names(spi5y.feb) == "spi1.5y"] <- "feb"
names(spi1y.mar)[names(spi1y.mar) == "spi1"] <- "mar"
names(spi5y.mar)[names(spi5y.mar) == "spi1.5y"] <- "mar"
names(spi1y.apr)[names(spi1y.apr) == "spi1"] <- "apr"
names(spi5y.apr)[names(spi5y.apr) == "spi1.5y"] <- "apr"
names(spi1y.may)[names(spi1y.may) == "spi1"] <- "may"
names(spi5y.may)[names(spi5y.may) == "spi1.5y"] <- "may"
names(spi1y.jun)[names(spi1y.jun) == "spi1"] <- "jun"
names(spi5y.jun)[names(spi5y.jun) == "spi1.5y"] <- "jun"
names(spi1y.jul)[names(spi1y.jul) == "spi1"] <- "jul"
names(spi5y.jul)[names(spi5y.jul) == "spi1.5y"] <- "jul"
names(spi1y.aug)[names(spi1y.aug) == "spi1"] <- "aug"
names(spi5y.aug)[names(spi5y.aug) == "spi1.5y"] <- "aug"
names(spi1y.sep)[names(spi1y.sep) == "spi1"] <- "sep"
names(spi5y.sep)[names(spi5y.sep) == "spi1.5y"] <- "sep"
names(spi1y.oct)[names(spi1y.oct) == "spi1"] <- "oct"
names(spi5y.oct)[names(spi5y.oct) == "spi1.5y"] <- "oct"
names(spi1y.nov)[names(spi1y.nov) == "spi1"] <- "nov"
names(spi5y.nov)[names(spi5y.nov) == "spi1.5y"] <- "nov"
names(spi1y.dec)[names(spi1y.dec) == "spi1"] <- "dec"
names(spi5y.dec)[names(spi5y.dec) == "spi1.5y"] <- "dec"



year1 <- spi1y.jan[, c("year","jan")]
year5 <- spi5y.jan[, c("year","jan")]
year1 <- merge(year1,spi1y.feb[, c("year","feb")], by=c("year"))
year5 <- merge(year5,spi5y.feb[, c("year","feb")], by=c("year"))
year1 <- merge(year1,spi1y.mar[, c("year","mar")], by=c("year"))
year5 <- merge(year5,spi5y.mar[, c("year","mar")], by=c("year"))
year1 <- merge(year1,spi1y.apr[, c("year","apr")], by=c("year"))
year5 <- merge(year5,spi5y.apr[, c("year","apr")], by=c("year"))
year1 <- merge(year1,spi1y.may[, c("year","may")], by=c("year"))
year5 <- merge(year5,spi5y.may[, c("year","may")], by=c("year"))
year1 <- merge(year1,spi1y.jun[, c("year","jun")], by=c("year"))
year5 <- merge(year5,spi5y.jun[, c("year","jun")], by=c("year"))
year1 <- merge(year1,spi1y.jul[, c("year","jul")], by=c("year"))
year5 <- merge(year5,spi5y.jul[, c("year","jul")], by=c("year"))
year1 <- merge(year1,spi1y.aug[, c("year","aug")], by=c("year"))
year5 <- merge(year5,spi5y.aug[, c("year","aug")], by=c("year"))
year1 <- merge(year1,spi1y.sep[, c("year","sep")], by=c("year"))
year5 <- merge(year5,spi5y.sep[, c("year","sep")], by=c("year"))
year1 <- merge(year1,spi1y.oct[, c("year","oct")], by=c("year"))
year5 <- merge(year5,spi5y.oct[, c("year","oct")], by=c("year"))
year1 <- merge(year1,spi1y.nov[, c("year","nov")], by=c("year"))
year5 <- merge(year5,spi5y.nov[, c("year","nov")], by=c("year"))
year1 <- merge(year1,spi1y.dec[, c("year","dec")], by=c("year"))
year5 <- merge(year5,spi5y.dec[, c("year","dec")], by=c("year"))

library("psych")
pairs.panels(year1, scale=TRUE, ci=TRUE, font.labels=45, cex.cor=20)
pairs.panels(year5, scale=TRUE, ci=TRUE, font.labels=15, cex.cor=5)

yearPca <- year1
yearPca$year <- NULL

# hauptkomponenten
cortest.bartlett(yearPca)
kmo <- KMO(yearPca)
kmo

res.p <- fa.parallel(yearPca, fa="pc")
fit <- princomp(yearPca, cor=FALSE)
plot(fit,type="lines")
summary(fit)

head(round(fit$scores, 2))
biplot(fit)
erg <- psych::principal(yearPca, rotate='none', nfactors=4)
erg$weights
summary(erg)
year5b <- cbind(year5, erg$scores)



mp <- ggplot()
mp + geom_line(aes(y=year5b$PC1, x=year5b$year), color="blue") +
  geom_line(aes(y=year5b$PC2, x=year5b$year), color="green") +
  geom_line(aes(y=year5b$PC3, x=year5b$year), color="red") +  
  geom_line(aes(y=year5b$PC4, x=year5b$year), color="brown") 
#+
#  geom_line(aes(y=year5b$PC5, x=year5b$year), color="magenta")   

filterYears = 50
#FFT for HDI
year5b$PC1.5y <- do_fft(year5b$PC1,filterYears)
year5b$PC2.5y <- do_fft(year5b$PC2,filterYears)
year5b$PC3.5y <- do_fft(year5b$PC3,filterYears)
year5b$PC4.5y <- do_fft(year5b$PC4,filterYears)

mp <- ggplot()
mp + geom_line(aes(y=year5b$PC1.5y, x=year5b$year), color="blue") +
  geom_line(aes(y=year5b$PC2.5y, x=year5b$year), color="green") +
  geom_line(aes(y=year5b$PC3.5y, x=year5b$year), color="red") +  
  geom_line(aes(y=year5b$PC4.5y, x=year5b$year), color="brown")


spi_ts <- ts(spi_all$spi1, frequency=12, start=c(1500,1))
plot.ts(spi_ts)

spi_ts.components <- decompose(spi_ts)
plot(spi_ts.components)


pci1_ts <- ts(year5b$PC1, frequency=58, start=c(1500,1))
pci1_ts.components <- decompose(pci1_ts)
plot(pci1_ts.components)

pca1y <- year1
pca1y$year <- NULL
pca5y <- year5
pca5y$year <- NULL
# do pca on 50y, but apply on 1y
res1 <- prcomp(pca5y)
summary(res1)
res1$rotation
pred <- as.data.frame(predict(res1, newdata=pca1y))
pci1_ts <- ts(pred$PC2, frequency=58, start=c(1500,1))
pci1_ts.components <- decompose(pci1_ts)
plot(pci1_ts.components)


# do pca on 1y, but apply on 50y
res2 <- prcomp(pca1y)
summary(res2)
res2$rotation
pred2 <- as.data.frame(predict(res2, newdata=pca5y))
pci1_5ts <- ts(pred2$PC1, frequency=58, start=c(1500,1))
pci1_5ts.components <- decompose(pci1_5ts)
plot(pci1_5ts.components)
#install.packages("ggplot2")
require("ggplot2")
library("markovchain")

setwd("C:/Code/climData/glaser2019")

dfHhi <- read.csv("csv/hhi_1500_2019.csv", sep=",", na = "NA")

pic <- dfHhi$hhi
len <- length(pic)
## mirror pic
pic <- append(pic, pic)
for(i in 1:len) {
  pic[i+len] <- pic[1+len-i]
}

frq <- fft(pic, inverse = FALSE)

frq1 <- frq
filterYears = 0.1   #filter 1m
start = round(len/(12.0*filterYears))
stop  = round(2.0*len-start)
frq1[start:stop] <- 0.0 
pic1 <- Re(fft(frq1, inverse = TRUE)/length(frq1))
pic1 <- pic1[1:len]
dfHhi$hhi1 <-  pic1

frq5 <- frq
filterYears = 5   #filter 5y
start = round(len/(12.0*filterYears))
stop  = round(2.0*len-start)
frq5[start:stop] <- 0.0 
pic5 <- Re(fft(frq5, inverse = TRUE)/length(frq5))
pic5 <- pic5[1:len]
dfHhi$hhi5 <-  pic5


dfHhi$rhi <- dfHhi$hhi
dfHhi$rmhi <- dfHhi$hhi
for(i in 1:length(dfHhi$rhi)) {
  dfHhi$rhi[i] = 'd4'
  if (dfHhi$hhi1[i] > -3.0) {dfHhi$rhi[i] = 'd3'}
  if (dfHhi$hhi1[i] > -2.0) {dfHhi$rhi[i] = 'd2'}
  if (dfHhi$hhi1[i] > -1.5) {dfHhi$rhi[i] = 'd1'}
  if (dfHhi$hhi1[i] > -1.0) {dfHhi$rhi[i] = 'n0'}
  if (dfHhi$hhi1[i] >  1.0) {dfHhi$rhi[i] = 'w1'}
  if (dfHhi$hhi1[i] >  1.5) {dfHhi$rhi[i] = 'w2'}
  if (dfHhi$hhi1[i] >  2.0) {dfHhi$rhi[i] = 'w3'} 
  if (dfHhi$hhi1[i] >  3.0) {dfHhi$rhi[i] = 'w4'}
  dfHhi$rmhi[i] <- paste('m',dfHhi$month[i],':',dfHhi$rhi[i], sep='')
}
dfHhi$hstate  <- dfHhi$hhi
dfHhi$hstate[1] <- paste('n0','-',dfHhi$rhi[1], sep='')
dfHhi$mhstate <- dfHhi$hhi
dfHhi$mhstate[1] <- paste('m',dfHhi$month[12],':','n0','-','m',dfHhi$month[1],':',dfHhi$rhi[1], sep='')
for(i in 2:length(dfHhi$hstate)) {
  dfHhi$hstate[i] <- paste(dfHhi$rhi[i-1],'-',dfHhi$rhi[i], sep='')
  dfHhi$mhstate[i] <- paste('m',dfHhi$month[i-1],':',dfHhi$rhi[i-1],'-','m',dfHhi$month[i],':',dfHhi$rhi[i], sep='')
}
monthStates <- c("m1","m2","m3","m4","m5","m6","m7","m8","m9","m10","m11","m12")
humidityStates <- c("w4", "w3", "w2", "w1", "n0", "d1", "d2", "d3", "d4")
byRow <- TRUE
humidityMatrix  <- matrix(rep( 0.0, len=length(humidityStates)^2), nrow=length(humidityStates))
for(row in 1:length(humidityStates)) {
  sumRow <- 0.0
  for(col in 1:length(humidityStates)) {
    trans <- paste(humidityStates[row],'-',humidityStates[col], sep='')
    count <- length(subset(dfHhi, dfHhi$hstate==trans)$hstate)
    humidityMatrix[row,col] <- count
    sumRow <- sumRow + count
  }
  if(sumRow > 0.0)  {
    for(col in 1:length(humidityStates)) {
      humidityMatrix[row,col] <- humidityMatrix[row,col] / sumRow
    } 
  }
}

mcHumidity <- new("markovchain", states = humidityStates, byrow = byRow,
                  transitionMatrix = humidityMatrix, name = "Humidity")

initialState <- c(0, 0, 0, 0, 1, 0, 0, 0, 0)
after1000month <- initialState * (mcHumidity ^ 1000)
transitionProbability(mcHumidity, "w4", "w3")

print(mcHumidity)
show(mcHumidity)
steadyStates(mcHumidity) #endless time

simHumidity <- rmarkovchain(n = 12*10, object = mcHumidity, t0 = "n0")

sequence <- c("a", "b", "a", "a", "a", "a", "b", "a", "b", "a", "b", "a", "a", 
              "b", "b", "b", "a")        
sequenceMatr <- createSequenceMatrix(dfHhi$rhi, sanitize = FALSE)

mcFitMLE <- markovchainFit(data = dfHhi$rhi)
mcFitBSP <- markovchainFit(data = dfHhi$rhi, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")

sequenceMatr <- createSequenceMatrix(dfHhi$rmhi, sanitize = FALSE)
mcFitMLE <- markovchainFit(data = dfHhi$rmhi)
mcFitBSP <- markovchainFit(data = dfHhi$rmhi, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")

steadyStates(mcFitBSP$estimate)
simHumidity <- rmarkovchain(n = 12*10, object = mcFitBSP$estimate, t0 = "m1:n0")

verifyMarkovProperty(dfHhi$rmhi)
verifyEmpiricalToTheoretical(data=dfHhi$rhi,object=mcFitBSP$estimate)
verifyHomogeneity(inputList=list(dfHhi$rmhi[3601:6000], dfHhi$rmhi[1:2400]),verbose=TRUE)

## create monthly
mcFitBSP <- markovchainFit(data = dfHhi$rmhi, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")

from = "d2"
to = "d1"

props <- monthStates
mon <- monthStates 
for(i in 1:length(monthStates)) {
  start = paste(monthStates[i],':',from, sep='')
  stop  = paste(monthStates[(i+1)%%length(monthStates)],':',to, sep='')  
  props[i] = transitionProbability(mcFitBSP$estimate, start, stop)
  mon[i] <- 0.01*i
}


mp <- ggplot()
mp + 
  theme_classic() +
  geom_point(aes(y=props, x=mon)) 
 
## 100 years

from = "w1"
to = "w2"


result <- data.frame(years = c(1:9))
mp <- ggplot(result) + theme_classic() +
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(1500,2020))  


for (from in humidityStates) {
  for (to in humidityStates) {
  result$prop <- c(1:9)    
for (i in 1:9) {
  yS = 1450 + 50*i
  yE = 1550 + 50*i
  result$years[i] <- 1500 + 50*i
  seq <- subset(dfHhi, dfHhi$year>yS & dfHhi$year<yE)
  mcFitBSP <- markovchainFit(data = seq$rhi, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")
  steadyStates(mcFitBSP$estimate)
  result$prop[i] <-transitionProbability(mcFitBSP$estimate, from, to)
  
}
  newName <- paste(from,'-',to, sep='')
  names(result)[names(result) == 'prop'] <- newName 
  mp <- mp + geom_line(aes(y=!!result[,newName], x=result[,"years"])) 
  
  }
}


### only look for rsults

result <- data.frame(years = c(1:9))
for (i in 1:9) {
  yS = 1450 + 50*i
  yE = 1550 + 50*i
  result$years[i] <- 1500 + 50*i
  seq <- subset(dfHhi, dfHhi$year>yS & dfHhi$year<yE)
  mcFitBSP <- markovchainFit(data = seq$rhi, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")
  states <- steadyStates(mcFitBSP$estimate)
  for(col in colnames(states)){
     result[i,col] <- states[,col]
  }
}

## continous
result <- data.frame(years = c(1600:2018))
for (yE in 1600:2018) {
  yS = yE - 100
  result$years[yE-1599] <- yE - 50
  seq <- subset(dfHhi, dfHhi$year>yS & dfHhi$year<yE)
  mcFitBSP <- markovchainFit(data = seq$rhi, method = "bootstrap", nboot = 5, name = "Bootstrap Mc")
  states <- steadyStates(mcFitBSP$estimate)
  for(col in colnames(states)){
    result[yE-1599,col] <- states[,col]
  }
}


mp <- ggplot(result) + theme_classic() +
  scale_y_continuous(limits=c(0,1))+
  scale_x_continuous(limits=c(1500,2020)) +
  geom_line(aes(y=result$d4, x=result$years, col="iD4")) +
  geom_line(aes(y=result$d3, x=result$years, col="hD3")) +  
  geom_line(aes(y=result$d2, x=result$years, col="gD2")) +
  geom_line(aes(y=result$d1, x=result$years, col="fD1")) +
  geom_line(aes(y=result$n0, x=result$years, col="eN0")) +
  geom_line(aes(y=result$w1, x=result$years, col="dW1")) +
  geom_line(aes(y=result$w2, x=result$years, col="cW2")) +
  geom_line(aes(y=result$w3, x=result$years, col="bW3")) +
  geom_line(aes(y=result$w4, x=result$years, col="aW4")) 
mp

result$wavg <- (result$w4*4 + result$w3*3 + result$w2*2 + result$w1*1)
result$davg <- 0-(result$d4*4 + result$d3*3 + result$d2*2 + result$d1*1) 
result$avg <- (result$davg + result$wavg)/2 

mp <- ggplot(result) + theme_classic() +
  scale_y_continuous(limits=c(-1.5,1.5))+
  scale_x_continuous(limits=c(1500,2020)) +
  geom_line(aes(y=result$wavg, x=result$years, col="wavg")) +
  geom_line(aes(y=result$avg, x=result$years, col="avg")) +
  geom_line(aes(y=result$davg, x=result$years, col="davg")) 
mp              



mp <- ggplot(dfHhi, aes(year, month))
mp + geom_raster(aes(fill=hhi1))+
  #theme_classic(base_size=80) +
  theme_classic() +
  scale_y_continuous(breaks=c(1,6,12))+
  scale_x_continuous(limits=c(1500,2020)) +  
  scale_fill_gradient2(low="#AA6010", mid="#FCF0C2", high="#23AB30") +
  theme( legend.key.width = unit(2,"cm")) +
  geom_line(aes(y=6+2*dfHhi$hhi5, x=dfHhi$time)) +
  #geom_line(aes(y=10+0.5*dfHhi$hhi, x=dfHhi$time)) +
  guides(fill=guide_legend(title="PI", reverse = TRUE))  

library("psych")
pairs.panels(result)

cr <- cor(result)
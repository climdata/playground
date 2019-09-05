require("ggplot2")

py <- read.csv("https://raw.githubusercontent.com/climdata/baur/master/csv/baur_monthly.csv", sep=",")
py2 <- subset(py, !is.na(py$temperature))



pt1 <- data.frame()
pt0 <- read.csv("https://raw.githubusercontent.com/climdata/glaser2010/master/csv/t_1500_2xxx.csv", sep=",", na = "NA")
monthNames = c("jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec")
for(i in 1:length(monthNames)) {
  ptnew <- data.frame(year = pt0$year, month = i)
  temp <- pt0[,monthNames[i]]
  ptnew$temperature <- temp
  pt1 <- rbind(pt1, ptnew)
}

pt1$time = pt1$year + (pt1$month+0.5)/12.0
pt1 <- pt1[order(pt1$time),]


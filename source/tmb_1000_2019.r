require("ggplot2")
require(gridExtra)

tmb <- read.csv("https://raw.githubusercontent.com/climdata/glaser2019/master/csv/d_1000_2019.csv", sep=",")

tmb$rel <- tmb$drought/(tmb$all + 0.00000001)


mp1 <- ggplot() +
  theme_classic(base_size=80) +
  coord_cartesian(ylim=c(0,300)) +
  labs(x="Year", y="n", title="", subtitle="") +
  geom_line(aes(y=tmb$drought, x=tmb$year), color="#0000AA", size=3) 



mp2 <- ggplot() +
  theme_classic(base_size=80) +
  labs(x="Year", y="%", subtitle="") +
  geom_line(aes(y=100*tmb$rel, x=tmb$year), color="#0000AA", size=3) 

p <- grid.arrange(mp1, mp2, nrow=2)
p

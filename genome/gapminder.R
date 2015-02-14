setwd("c:/artr/genome")
library(devtools)
find_rtools
install_github("jennybc/gapminder")
library(gapminder)
data(gapminder)
head(gapminder)
x<-gapminder[gapminder$year=="1952",4]
hist(x)
lf40 <- mean(x <= 40)
lf60 <- mean(x <= 60)
lf60-lf40
lgy<-log10(gapminder[gapminder$year=="1952",5])
#lgy<- log10(y)
hist(lgy)
sd(lgy)
qqnorm(ylg)
ml<-mean(lgy)
sl<- sd(lgy)
z<-(lgy-ml)/sl
n <- length(lgy)
F <- function(q)pnorm(q,mean=mean(lgy),sd=sd(lgy))
log10(range(gapminder[,5]))
head(pnorm(z,mean=mean(z),sd=sd(z)))

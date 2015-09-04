rm(list=ls())
setwd("c:/code/rfiles/rdata")
library("dplyr","ggplot2","knitr")
wrh<- read.csv("wrhdtl0715.csv")
psqft<-cbind(wrh[,"pno"],wrh[,2:18]/wrh[,"sqft"])
punit<-cbind(wrh[,"pno"],wrh[,2:18]/wrh[,"units"])
summary(wrh[,"margin"])
boxplot(wrh[,"margin"])
hist(wrh[,"margin"])
d<- density(wrh[,"margin"])
plot(d)
top1 <-which(wrh[,"margin"]>.45)
wrh[top1,c("pno","margin")]
psqft[top1,]
mnsqft<- colMeans(psqft[top1,])

mnsqpsqft

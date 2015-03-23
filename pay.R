setwd("c:/artr")
list.files()
pay <- read.csv("payhist.csv")
head(pay)
summary(pay)
pay$Prop <- factor(pay$Prop)
p<- pay$Prop
pay$Vendor <- factor(pay$Vendor)
summary(pay)
s
mean(pay$Amount)
nrow(pay)

x<- plot(x)
x<-tapply(pay$Amount,pay$Vendor,length,simplify=TRUE)
x
stem(x)

s<- split(pay,pay$Prop)
lapply(s,sum,s$Amount)

class(x)
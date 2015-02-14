setwd("c:/artR/genome")
tab<- read.csv("msleep_ggplot2.txt")
c(tab$sleep_total,1000)
plot(tab$brainwt,tab$sleep_total)
plot(tab$brainwt,tab$sleep_total,log="x")
summary(tab)
mean(tab[tab$sleep_total>18,"sleep_total"])
which(tab$sleep_total>18)
tab$sleep_total[which(tab$sleep_total>18)[1]]
which(tab$sleep_total>18 & tab$sleep_rem<3)
sort(tab$sleep_total)
order(tab$sleep_total)
tab$sleep_total[order(tab$sleep_total)]
rank(c(1,2,2,3,4))
rank(tab$sleep_total)
match("Cotton rat",tab$ï..name)
vec<- c("red","blue", "red","green","green","yellow","orange")
fac<- factor(vec)
fac
levels(fac)
head(tab)
table(tab$order)
s<- split(tab$sleep_total,tab$order)
s
s[[17]]
mean(s[["Rodentia"]])
     
lapply(s,mean)
sapply(s,mean)
tapply(tab$sleep_total,tab$order,sd)

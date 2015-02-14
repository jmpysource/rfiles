setwd("c:/artr/genome")
dat<- read.csv("femaleMiceWeights_csv.txt")
head(dat)
str(dat)
cont<- dat[dat$diet=="chow",2]
fat <- dat[dat$diet=="hf",2]
cm<-mean(cont)
fm<-mean(fat)
fm-cm
colnames(dat)
colnames(dat)<-c("diet","wght")
s<- split(dat[,2],dat[,1])
stripchart(s,vertical=TRUE,col=1:2)
abline(h=sapply(s,mean),col=1:2)
which(cont>fm)
fat

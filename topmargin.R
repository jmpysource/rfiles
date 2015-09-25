rm(list=ls()) # clear out objects
setwd("c:/code/rfiles/rdata")
library("dplyr")
library("knitr")
library("ggplot2")
options(scipen=999)
wrh<- read.csv("currentdtl.csv")
md<-which(wrh$pno==280)#find Madalyn Landing - less than 12 months
wrh<- wrh[-md,]# remove Madalyn
row.names(wrh)<- wrh$pno
wrh$margin<- wrh$noiar/wrh$rev  # recalc margin just ot be sure
wrh$avgunitsize<- wrh$sqft/wrh$units
tot<- sapply(wrh[,c(2:19,21:26)],sum)#get totals for number columns
totmargin<-sum(wrh$noiar)/sum(wrh$rev)#get total margin
psqft<-cbind(wrh$pno,wrh[,2:19]/wrh$sqft,wrh$avgunitsize,wrh$margin) #create per square foot dataframe
colnames(psqft)[colnames(psqft)=="wrh$margin"]<-"margin"
colnames(psqft)[colnames(psqft)=="wrh$avgunitsize"]<-"AvgUnitSize"
colnames(psqft)[colnames(psqft)=="wrh$pno"]<-"prop#"
punit<-cbind(wrh$pno,wrh[,2:19]/wrh$units,wrh$avgunitsize,wrh$margin)#create per unit dataframe
colnames(punit)[colnames(punit)=="wrh$margin"]<-"margin"
colnames(punit)[colnames(punit)=="wrh$avgunitsize"]<-"AvgUnitSize"
colnames(punit)[colnames(punit)=="wrh$pno"]<-"prop#"

sumview<- c("gpr","rent","rev","totexp","noiar")
sview<- c("gpr","rent","rev","totexp","margin")
rview<- c("gpr","vacy","rent","othinc","rev","totexp","margin")

print(summary(wrh$margin,digits=3))
boxplot(wrh$margin)
#hist(wrh$margin)
hist(wrh$margin, breaks=c(.2,.25,.30,.35,.40,.45,.50,.55,.6),main="Margins of WRH Properties",xlab="Margin(NOIAR/Revenue)",ylab="No. of Properties")
# Report 1
# Margins by property
wrh$Margin<-format(wrh$margin,digits=3) #add a formatted margin - use a capital M
sum_margin<-wrh[,c("pno","prop","Margin","noiar","margin")] #create a smaller dataframe
# t<- which (sum_margin$prop=="Total") # Find the Total Row to remove it
# sum_margin<-sum_margin[-t,] # Remove the Total Row
sum_margin<- sum_margin[order(sum_margin$Margin,decreasing = TRUE),]#Create a sorted DF
print(format(sum_margin,justify="left",big.mark=","),print.gap=3) #Print it
print("Totals",quote="FALSE")
print(format(tot[c(6,18,23)],justify="left",big.mark=",",print.gap=3),quote="FALSE") #Print it
print ("Total Margin:",quote=FALSE)
print(totmargin)

#Identify Top 5 and create DF
cutoff<-min(sum_margin[1:5,"Margin"])
top5 <-which(wrh$margin>=cutoff)
wrhtop5<-wrh[top5,]
wrhtop5<-wrhtop5[order(wrhtop5$Margin,decreasing = TRUE),]
sum5<- wrhtop5[,c("prop","state","rev","noiar","units","sqft")]
sum5$revunit<-sum5[,3]/sum5[,5]
sum5$noiarunit<-sum5[,4]/sum5[,5]
sum5$revsqft<-sum5[,3]/sum5[,6]
sum5$noiarsqft<-sum5[,4]/sum5[,6]
sum5$avgunitsqft<-sum5[,6]/sum5[,5]
print(format(sum5,justify="left",big.mark=","),print.gap=3) #Print it
psqft[top5,]
mnsqft<- colMeans(psqft[top5,])


brk<-c(.25,.5,.75)
qnt<- quantile(wrh$margin,brk)
low<-wrh$margin<=qnt[1]
mid<-wrh$margin>qnt[1] & wrh$margin<=qnt[2]
high<-wrh$margin>qnt[2] & wrh$margin<=qnt[3]
topq<-wrh$margin>qnt[3]
#high<-wrh$margin>qnt[2] & wrh$margin<cutoff
#topq<-wrh$margin>=cutoff
print(wrh[low,c("prop","margin","units","sqft")],digits=3)
print(wrh[mid,c("prop","margin")],digits=3)
print(wrh[high,c("prop","margin")],digits=3)
print(wrh[topq,c("prop","margin")],digits=3)
labl<-c("low","mid","high","topq")
lowmean<-colMeans((psqft[low,sview]))
midmean<-colMeans((psqft[mid,sview]))
highmean<-colMeans((psqft[high,sview]))
topmean<-colMeans((psqft[topq,sview]))
meancat<-data.frame(low=lowmean)
meancat$mid<-midmean
meancat$high<-highmean
meancat$top<-topmean


bystate<- group_by(wrh,state)
bystate<- mutate(bystate,wtdmarg=rev/sum(rev)*margin)
sumstate<-summarise(bystate, count=n(), revenue=sum(rev), noiar=sum(noiar), units=sum(units), sqfeet=sum(sqft))
sumstate<- mutate(sumstate,marg=noiar/revenue)
sumstate$marg<-format(sumstate$marg,digits=3)
print(format(sumstate,justify="left",big.mark=","),print.gap=3)



labl<-c("low","mid","high","topq")
lowmeanu<-colMeans((punit[low,sview]))
midmeanu<-colMeans((punit[mid,sview]))
highmeanu<-colMeans((punit[high,sview]))
topmeanu<-colMeans((punit[topq,sview]))
meancatu<-data.frame(low=lowmeanu)
meancatu$mid<-midmeanu
meancatu$high<-highmeanu
meancatu$top<-topmeanu
print(meancatu,digits=4)

srt<-arrange(psqft[,c("prop#",sview)],desc(margin))
#print (format(srt,justify="left",big.mark=",", digits=3))
#summary(srt$rev)
#summary(srt$totexp)
boxplot(srt$totexp,srt$rev,names=c("OprExp$/Sqft","Rev$/SqFt"))

srt<-arrange(psqft[,c("prop#",sview)],desc(margin))
print (format(srt,justify="left",big.mark=",", digits=3))

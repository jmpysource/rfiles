setcwd=("c:/artR")
library(dplyr)
groupsummary<-function(grpframe){
  summarise(grpframe,Prps=n(),Units=sum(units),'Unit%'=sum(pctunits),Rev=sum(rev),'Rev%'=sum(pctrev),NOIAR=sum(noiar),'Noiar%'=sum(pctnoiar))
   }
len<- function(x) {
  length(x)
   }
topsummary<- function(frame){
  summarise(frame,Props=n(),Units=sum(units),Revenue=sum(rev),NOIAR=sum(noiar),CashFlowAfterDS=sum(cfds))
  }
marginsummary<-function(frame){
  summarise(frame,props=n(),Rev=sum(rev),NOIAR=sum(noiar),MarginAvg=mean(margin))
  }
margview<-c(1,2,12,3,16,21)
uview<-c(2,3,8,9,10,11)
debtview<-c(2,3,4,5,17,29)

unitsummary<- function(unit){
  summarise(unit,Units=sum(units),bedrmAvg=mean(bedavg),sqftAvg=mean(sqftavg),rentAvg=mean(rentunit),rentsqftavg=mean(rentsqft))
  }

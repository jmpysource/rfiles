setcwd=("~/data")
library(dplyr)
groupsummary<-function(grpframe){
  summarise(grpframe,Prps=n(),Rev=sum(rev/1000),'Rev%'=sum(pctrev),NOIAR=sum(noiar/1000),"NOIAR%"=sum(pctnoi),Units=sum(units),'Unit%'=sum(pctunits))
   }
len<- function(x) {
  length(x)
   }
#topsummary gives totals for an ungrouped data frame-prop count,units,revenue,noi,cashflow
topsummary<- function(frame){
  summarise(frame,Props=n(),Revenue=sum(rev/1000),NOIAR=sum(noiar/1000),CashFlow=sum(cflow/1000),Units=sum(units))
  }
#marginsummary also works on an ungrouped dataframe - propcount,revenue,noi,average margin
marginsummary<-function(frame){
  summarise(frame,props=n(),Rev=sum(rev/1000),NOIAR=sum(noiar/1000),MarginAvg=mean(margin))
  }
margview<-c(1,2,12,3,16,21)
uview<-c(2,3,8,9,10,11)
debtview<-c(2,3,4,5,17,29)

unitsummary<- function(unit){
  summarise(unit,Units=sum(units),bedrmAvg=mean(bedavg),sqftAvg=mean(sqftavg),rentAvg=mean(rentunit),rentsqftavg=mean(rentsqft))
  }

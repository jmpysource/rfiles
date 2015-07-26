# get wrhcashflow
options(width = 150)
library(dplyr)
library(knitr)
#setwd("c:/code/rfiles")
#setwd("~/data")
setwd("c:/artR")
wrh <- read.csv("wrhcurrent.csv")
wrh$pno <- as.factor(wrh$pno)
wrh$st <- as.factor(wrh$st)
wrh$revunit <- wrh$rev/wrh$units
wrh$noiunit <- wrh$noiar/wrh$units
wrh$revft <-wrh$rev/wrh$sqft
wrh$noift <- wrh$noiar/wrh$sqft
rownames(wrh) <- wrh$pno
#identify columns that can be totaled
#these may change as you adjust the file
totalcols <- c(rev=7,noiar=8,debtsvc=9,cflow=10,reno=11,corpexp=12,netcf=13,sqft=17,units=6)
#set up a vector with portfolio totals
totals<-apply(wrh[,totalcols],2,sum)
print ("Portfolio Totals for Trailing 12 Months",quote=FALSE)
print ("",quote = FALSE)
print (format(totals,big.mark = ","),quote=FALSE)
wrh$pctrev <- wrh$rev/totals["rev"]
wrh$pctnoi <- wrh$noiar/totals["noiar"]
wrh$pctunits <- wrh$units/totals["units"]
wrh$pctsqft <- wrh$sqft/totals["sqft"]
wrh$debtcvg <- wrh$noiar/wrh$debtsvc
wrh$debtrev <-wrh$debtsvc/wrh$rev
wrh$debtnoi <-wrh$debtsvc/wrh$noiar
cfview <- c(2,7,8,9,10,6) #cash flow report view- with revenue
unitview <- c(2,7,8,22,23,24,25,6) #per unit and per sqft view
pctview <- c(2,7,8,26,27,28,29)
genview <- c(2,4,7,8,10,15,6)
debtview <-c(2,7,8,9,10,30,31,32,19,18)
#head(wrh)
#str(wrh)
#groups for dplyr
by_state <- group_by(wrh,st)
by_area  <- group_by(wrh,loc)
by_mkt <- group_by(wrh,mkt)
by_lender <- group_by(wrh,lender)
# Build states for filter
states <- levels(wrh$st)
for (i in states){
     p<-filter(wrh[,genview],wrh$st == i);p<-arrange(p,prop)
     cat("State   :");print(i,quote = FALSE);print;print(format(p,justify="left",big.mark=","),print.gap=3)
     sm <- format(colSums(p[,c(3,4,5,7)]),big.mark=","); print("Totals:",quote=FALSE);print(sm,quote="FALSE",print.gap=4);print("")}


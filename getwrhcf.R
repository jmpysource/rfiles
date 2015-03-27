# get wrhcashflow
library(dplyr)
library(knitr)
setwd("c:/code/rfiles")
#setwd("~/data")
wrh <- read.csv("wrhcurrent.csv")
wrh$pno <- as.factor(wrh$pno)
wrh$revunit <- wrh$rev/wrh$units
wrh$revft <-wrh$rev/wrh$sqft
wrh$noiunit <- wrh$noiar/wrh$units
wrh$noift <- wrh$noiar/wrh$sqft
rownames(wrh) <- wrh$pno
#identify columns that can be totaled
#these may change as you adjust the file
totalcols <- c(units=6,rev=7,noiar=8,debtsvc=9,cflow=10,reno=11,corpexp=12,netcf=13,sqft=17)
#set up a vector with portfolio totals
totals<-apply(wrh[,totalcols],2,sum)
print (totals)
wrh$pctrev <- wrh$rev/totals["rev"]
wrh$pctnoi <- wrh$noiar/totals["noiar"]
wrh$pctunits <- wrh$units/totals["units"]
wrh$pctsqft <- wrh$sqft/totals["sqft"]
wrh$pctdebtsvc <- wrh$debtsvc/totals["debtsvc"]
wrh$debtrev <-wrh$debtsvc/wrh$rev
wrh$debtnoi <-wrh$debtsvc/wrh$noiar
# columns used for views
# [1] "pno"        "prop"       "st"         "mkt"        "loc"        "units"      "rev"       
#[8] "noiar"      "debtsvc"    "cflow"      "reno"       "corpexp"    "netcf"      "margin"    
#[15] "occ"        "eocc"       "sqft"       "period"     "revunit"    "revft"      "noiunit"   
#[22] "noift"      "pctrev"     "pctnoi"     "pctunits"   "pctsqft"    "pctdebtsvc" "debtrev"   
#[29] "debtnoi" 
cfview <- c(2,7,8,9,10,6) #cash flow report view- with revenue
unitview <- c(2,7,8,19,21,20,22,6,17) #per unit and per sqft view
pctview <- c(2,7,8,23,24,27,25,26)
genview <- c(2,4,7,8,10,15,6)
debtview <-c(2,7,8,9,10,18,29,30,31,19)
#head(wrh)
str(wrh)
#groups for dplyr
by_state <- group_by(wrh,st)
by_area  <- group_by(wrh,loc)
by_mkt <- group_by(wrh,mkt)
by_lender <- group_by(wrh,lender)
# Build states for filter
states <- levels(wrh$st)
for (i in states){
p<-filter(wrh[,genview],wrh$st == i);p<-arrange(p,mkt);print(i);print(p,big.mark=",")}

setwd("c:/code/R_files")
options(scipen=4)
status<- read.csv("weekreport.csv")
str(status)
library ("dplyr")
by_reg <- group_by(status,regional)
summarise(by_reg,runitr=sum(units))


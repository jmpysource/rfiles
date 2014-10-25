setwd("c:/artr")
library (dplyr)

taxfileall <- read.csv("taxdeprec2013.csv")
head(taxfileall)
group <- levels(taxfileall$group)
groupwk <- group[c(2,3,4,5,9,10)]
tax1 <- taxfileall[,c(2,3,4,8,9,10,11,12,13,14)]
head (tax1)
tax <- tax1[tax1$taxnbv > 0,]
tax <- tax[tax$remyears > 0,]	
tax <- omit.na(tax)
tax$expreason <- 'None'
low <- which (tax$cost < 201)
supervision <- grep("uper",tax$descr)
tax[low,"expreason"] <- 'repair below $200'
tax[supervison,"expreason"] <- 'internal supervison cost'
tax$type <- "None"

set_type <- function(term,name){
    cat <- grep(term,tax$descr)
    tax[cat,"type"] <<- name
    return (cat)
    }

exportdescr <- function(dfcol){
    types<- levels(tax$descr)
    write.csv(types,"assetnames.csv")
    }

gettypefile <- function(csvfile){
     search<- read.csv("xrefgrep.csv",stringsAsFactors=FALSE)
     #search[,1]<- as.character(search[,1])
     #search[,2]<- as.character(search[,2])
     return (search)
     }

review <- c(7,1,12,2,3,4)

rep <- c('dishwasher','range','hood','icemaker','range','microwave','refrigerator','carpet')



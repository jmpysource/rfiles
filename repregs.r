#setwd("c:/artR")  # home directory
setwd("c:/code/R_files")  # work directory
library (dplyr) 

taxfileall <- read.csv("taxdeprec2013.csv") # read cleaned up tax depr file
head(taxfileall)
group <- levels(taxfileall$group)     #grab all tax categories
groupwk <- group[c(2,3,4,5,9,10)]     #just use categories you want
tax1 <- taxfileall[,c(2,3,4,8,9,10,11,12,13,14)] # get rid of columns you dont want
head (tax1)
tax <- tax1[tax1$taxnbv > 0,] # eliminate 0 or lower NBV
tax <- tax[tax$remyears > 0,] # eliminate fully depreciated assets
tax$svcdate <- as.character(tax$svcdate) #change date to character instead of level
tax$descr <- tolower(tax$descr)   #make all descriptions lower case for matching
tax <- omit.na(tax)               # eliminate incomplete records
tax <- tax[tax$group != "Loan Cost",] # not sure how these go through
tax <- tax[tax$propno != 375,]       #get rid of Cross Creek
tax$expreason <- 'None'              #Create and fill reason to expense column
low <- which (tax$cost < 201)        # identify under $200 cost items
#supervision <- grep("uper",tax$descr)
tax[low,"expreason"] <- 'repair below $200'
#tax[supervision,"expreason"] <- 'internal supervision cost'
tax$type <- "None"                 #Create and fill broad category 

set_type <- function(term,name){
    ## map search terms to a type name - eg. items containing pain map to paint
    cat <- grep(term,tax$descr)
    tax[cat,"type"] <<- name
    return (cat)
    }

exportdescr <- function(dfcol){
    ## Export asset descriptions to build grep table in excel
    types<- levels(tax$descr)
    write.csv(types,"assetnames.csv")
    }

gettypefile <- function(csvfile="xrefgrep.csv"){
     ## import xref file for grep search in set_type function
     search<- read.csv(csvfile,stringsAsFactors=FALSE)
     return (search)
     }
search <- gettypefile()  #create search file for grep function

review <- c(7,1,12,2,3,4) #critical columns for review
rep <- c('dishwasher','range','hood','icemaker','range','microwave','refrigerator','carpet ','doors','lighting','supplies','repair')

sendfile <- function(data,filenamecsv){
     ## write processed file for analysis in excel
     write.csv(data,filenamecsv)
     print ('file save complete')


#for (i in  1:x){
#    set_type(search[i,1],search[i,2])
#    }

ann_dep <- function (dframe){
     depestimate <- dframe$cost/dframe$life
     rem_calc <- dframe$remyears * depestimate
     #adj <- dframe$taxnbv/rem_calc
     #depestimate <- depestimate * adj
     return (depestimate)
     }

 


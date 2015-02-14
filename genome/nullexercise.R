setwd("c:/artr/genome")
pop<- read.csv("femaleControlsPopulation_csv.txt")
head(pop)
colnames(pop)<-"wght"

control<- sample(pop[,1],12)
mean(control)

n<- 10000  #number of trials
null <- vector("numeric",n)# create empty numeric vector - 

for (i in 1:n){
    control<- sample(pop[,1],12)
    treatment <- sample(pop[,1],12)
    null[i] <- mean(treatment)-mean(control)
}

diff <- 3.02
mean(null>diff)#p-value -trick :null>diff generates TRUE(1)/FALSE(0) values
# mean gives proportion (sum of values(1s for true)/number of values)
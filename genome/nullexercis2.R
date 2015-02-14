setwd("c:/artr/genome")
pop1 <- read.csv("femaleControlsPopulation_csv.txt")
pop <- pop1[,1]
mean(pop)
samplemean <- replicate(10000,mean(sample(pop,12)))
head(samplemean)
plot(samplemean)
null <- replicate(10000,mean(sample(pop,12)-mean(sample(pop,12))))
plot(null)
hist(null)
diff<-3.0208333
abline(v=diff,col="red")
abline(v=-diff,col="red")
mean(null>diff )
sum(null< v)
v<- -diff
v
null< v
mean(null < v)
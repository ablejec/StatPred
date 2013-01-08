###################################################
### chunk number 1: 
###################################################
options(width=75)
library(Hmisc)
library(xlsReadWrite)


###################################################
### chunk number 2: 
###################################################
data <- read.xls("../data/podatki0708.xls")
summary(data[,-1])
data <- data[data$spol=="F",]
attach(data)
n <- length(id)


###################################################
### chunk number 3: 
###################################################
par(mfrow=c(2,2))
xlim <- range(c(visina,razpon),na.rm=TRUE)*c(0.95,1.05)
barplot(table(starost),col="lightblue",main="starost")
hist(visina,col="lightblue",xlim=xlim)
rug(jitter(visina))
hist(teza,col="lightblue")
rug(jitter(teza))
hist(razpon,col="lightblue",xlim=xlim)
rug(jitter(razpon))



###################################################
### chunk number 4: 
###################################################



###################################################
### chunk number 5: 
###################################################

plot(sort(visina),1:n)
points(sort(visina),1:n)
v <- order(visina)
lines(visina[v],rank(visina)[v],pch=16,col="red",type="o",cex=1.2)
table(visina)
lines(as.numeric(names(table(visina))),table(visina),type="h",lwd=3)
boxplot(visina, horiz=TRUE,add=TRUE)




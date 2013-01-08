###################################################
### chunk number 1: 
###################################################
options(width=70)
#library(Hmisc)
#library(xlsReadWrite)


###################################################
### chunk number 2: 
###################################################
m <- 6
set.seed(1010)


###################################################
### chunk number 3: 
###################################################
n <- 120
x <- round(runif(n,0.5,6.5))
x
catln("Povpreèje:",mean(x))
catln("Varianca :",var(x))


###################################################
### chunk number 4: dunif
###################################################
barplot(table(x),main=paste("U( ",m," ),   n =",n))
abline(h=n/m,col="red",lwd=3 )


###################################################
### chunk number 5: 
###################################################
# priprava podatkov
cex=2
n <- 30
p <- 0.2
r <- 3
m <- round((r/p)*2)
Y <- matrix(as.numeric((runif(m*n)>=1-p)),n,m)


###################################################
### chunk number 6: binom
###################################################
nf <- layout(matrix(c(1,2),2,1),1,c(4,1))
#layout.show(nf)
par(mar=c(3,3,1,2))
plot(col(Y),n-row(Y),type="n",axes=FALSE,xlab="",ylab="")
abline(h=0:(n-1),col="grey")
points(col(Y),n-row(Y),pch=c(1,21)[Y+1],cex=c(.5,cex)[Y+1],col=c(1,1)[Y+1],bg="white")
axis(1)
title(paste("B(n = ",m,"  p = ",p, ")    E(X) = np = ",round(n*p,1)))
rti <- apply(Y,1,sum)
text(m+2,n:1-1,rti,xpd=TRUE,adj=1)
par(mar=c(3,3,1,2))
tbl <- table(rti)
plot(names(tbl),tbl,type="h",xlim=c(1,m),xlab="",ylab="",axes=FALSE,lwd=5)
axis(1)
catln("Povpreèje: ",round(mean(rti,na.rm=TRUE),2))
catln("Varianca : ",round(var(rti,na.rm=TRUE),2))


###################################################
### chunk number 7: geom
###################################################
r=1
nf <- layout(matrix(c(1,2),2,1),1,c(4,1))
#layout.show(nf)
par(mar=c(3,3,1,2))
plot(col(Y),n-row(Y),type="n",axes=FALSE,xlab="",ylab="")
abline(h=0:(n-1),col="grey")
points(col(Y),n-row(Y),pch=c(1,21)[Y+1],cex=c(.5,cex)[Y+1],col=c(1,1)[Y+1],bg="white")
axis(1)
title(paste("G( p = ",p, ")    E(X) = 1 / p = ",round(1/p,1)))
rti <- apply(Y,1,FUN=function(x,r) {which(cumsum(x)==r)[1]},r=r)
points(rti,n:1-1,pch=16,col="black",cex=cex*.75)
text(m+2,n:1-1,rti,xpd=TRUE,adj=1)
par(mar=c(3,3,1,2))
tbl <- table(rti)
plot(names(tbl),tbl,type="h",xlim=c(1,m),xlab="",ylab="",axes=FALSE,lwd=5)
axis(1)
catln("Povpreèje: ",round(mean(rti,na.rm=TRUE),2))
catln("Varianca : ",round(var(rti,na.rm=TRUE),2))


###################################################
### chunk number 8: nb
###################################################
rs <- 3
for(r in rs){
nf <- layout(matrix(c(1,2),2,1),1,c(4,1))
#layout.show(nf)
par(mar=c(3,3,1,2))
plot(col(Y),n-row(Y),type="n",axes=FALSE,xlab="",ylab="")
abline(h=0:(n-1),col="grey")
points(col(Y),n-row(Y),pch=c(1,21)[Y+1],cex=c(.5,cex)[Y+1],col=c(1,1)[Y+1],bg="white")
axis(1)
title(paste("NB(r = ",r,"  p = ",p, ")    E(X) = r / p = ",round(r/p,1)))
rti <- apply(Y,1,FUN=function(x,r) {which(cumsum(x)==r)[1]},r=r)
points(rti,n:1-1,pch=16,col="black",cex=cex*.75)
text(m+2,n:1-1,rti,xpd=TRUE,adj=1)
par(mar=c(3,3,1,2))
tbl <- table(rti)
plot(names(tbl),tbl,type="h",xlim=c(1,m),xlab="",ylab="",axes=FALSE,lwd=5)
axis(1)
catln("Povpreèje: ",round(mean(rti,na.rm=TRUE),2))
catln("Varianca : ",round(var(rti,na.rm=TRUE),2))
}


###################################################
### chunk number 9: exp
###################################################
r <- 1
lambda <- 5
beta <- 1/lambda
T <- seq(0,3,0.05)
fT=dexp(T,lambda)
rT=rexp(1000,lambda)
main <- substitute(paste("Exp( ",lambda," = ",ll," )"),list(ll=lambda))
hist(rT,prob=TRUE,col="lightblue",ylim=range(fT),xlab="T",main=main)
lines(T,fT,type="b",col="red",lwd=3)
catln(
"Beta     :",beta,"\n",
"Lambda   :",lambda,"\n",
"Povpreèje:",mean(rT),"\n",
"Varianca :",var(rT))


###################################################
### chunk number 10: gama
###################################################
r <- 4
lambda <- 5
beta <- 1/lambda
T <- seq(0,3,0.05)
fT=dgamma(T,r,scale=beta)
rT=rgamma(1000,r,scale=beta)
hist(rT,prob=TRUE,col="lightblue",xlab="T",ylim=range(fT),main=paste("gama(",r,",",lambda,")",sep=""))
lines(T,fT,type="b",col="red",lwd=3)
catln(
"r        :",r,"\n",
"Beta     :",beta,"\n",
"Lambda   :",lambda,"\n",
"Povpreèje:",mean(rT),"\n",
"Varianca :",var(rT))


###################################################
### chunk number 11: Poisson
###################################################
k <- 0:18
lambdas <- c(1,2,4,8)
if(length(lambdas>1)) par(mfrow=c(2,2))
for(lambda in lambdas) 
barplot(dpois(k,lambda),names.arg=k,space=0.5,
main=paste(expression(lambda)," = ",lambda),col="lightblue")


###################################################
### chunk number 12: sessionInfo
###################################################
cat(win.version(),"\n")
toLatex(sessionInfo())



###################################################
### chunk number 1: initialize
###################################################
options(width=50)
library(Hmisc)
#library(xlsReadWrite)


###################################################
### chunk number 2: Author:
###################################################
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################


###################################################
### chunk number 3: 
###################################################
lfn <- "ST2011.txt"


###################################################
### chunk number 4: 
###################################################
data <- read.table(file.path("../data",lfn),sep="\t",header=TRUE)
str(data)


###################################################
### chunk number 5: 
###################################################
data


###################################################
### chunk number 6: 
###################################################
opisne <-which(sapply(data,"class")=="factor")
length(opisne)
names(data)[opisne]


###################################################
### chunk number 7: 
###################################################
stevilske <- which(!sapply(data,"class")=="factor")
length(stevilske)
names(data)[stevilske]


###################################################
### chunk number 8: 
###################################################
summary(data[,1:6])


###################################################
### chunk number 9: 
###################################################
summary(data[,7:12])


###################################################
### chunk number 10: 
###################################################
par(mfrow=c(3,3))
for(i in stevilske) plot(data[,i],main=names(data)[i])


###################################################
### chunk number 11: 
###################################################
nas <- which(apply(data,1,function(x) any(as.numeric(x)==0)))
nas
if(length(nas)>0) data <- data[-nas,]
dim(data)


###################################################
### chunk number 12: 
###################################################
data <- data[data$starost<25,]


###################################################
### chunk number 13: 
###################################################
data <- data[data$roke>50,]


###################################################
### chunk number 14: 
###################################################
data[,"mati"] <- data[,"mati"]+100*(data[,"mati"]<100)
data[,"oce"] <- data[,"oce"]+100*(data[,"oce"]<100)


###################################################
### chunk number 15: 
###################################################
x <- data[,"majica"]
levels(x)
levels(x) <- gsub("(.*) ","\\1",levels(x))
levels(x)


###################################################
### chunk number 16: 
###################################################
levels(data[,"spol"]) <- c("zenski","moski")
levels(data[,"oci"]) <-c("svetle","temne")
levels(data[,"lasje"])<-c("svetli","temni")


###################################################
### chunk number 17: 
###################################################
(data[,"majica"] <-ordered(x,levels=c("XXS","XS","S","M","L","XL")))


###################################################
### chunk number 18: 
###################################################
data


###################################################
### chunk number 19: 
###################################################
summary(data[,1:6])


###################################################
### chunk number 20: 
###################################################
summary(data[,7:12])


###################################################
### chunk number 21: 
###################################################
par(mfrow=c(2,2))
for(i in opisne) plot(data[,i],main=names(data)[i],xlab=names(data)[i],col="lightblue")


###################################################
### chunk number 22: 
###################################################
par(mfrow=c(3,3))
for(i in stevilske) hist(data[,i],main=names(data)[i],xlab=names(data)[i],col="lightblue")


###################################################
### chunk number 23: 
###################################################
attach(data)


###################################################
### chunk number 24: boxvis
###################################################
boxplot(visina~spol, col=c("pink","lightblue"),ylab="visina")
rug(visina[spol=="zenski"],side=2,col="pink",lwd=2)
rug(visina[!spol=="zenski"],side=4,,col="lightblue",lwd=2)


###################################################
### chunk number 25: 
###################################################
boxplot(visina~spol, col=c("pink","lightblue"),ylab="visina")
rug(visina[spol=="zenski"],side=2,col="pink",lwd=2)
rug(visina[!spol=="zenski"],side=4,,col="lightblue",lwd=2)


###################################################
### chunk number 26: 
###################################################
y <- visina
(n <- tapply(y,spol,length))
(xbar <- tapply(y,spol,mean))
(s <- tapply(y,spol,sd))


###################################################
### chunk number 27: 
###################################################
alpha <- 0.01
(v <- sort(s^2))
ns <- as.vector(n[order(s)]) # da se znebim imen
(F <- as.vector(v[2]/v[1]))
(df1 <-ns[2]-1)
(df2 <-ns[1]-1)
(F.krit <- qf(1-alpha,df1,df2))
(p <- 1-pf(F,df1,df2))
if(F<F.krit) cat("Varianci NISTA statistièno znaèilno razlièni (p =",round(p,3),").\n") else
cat("Varianci STA  statistièno zanèilno razlièni (p <",alpha,") (p =",round(p,3),").\n")


###################################################
### chunk number 28: critvisF
###################################################
x <- seq(0,max(F,F.krit)*1.5,length=100)
plot(x,df(x,df1,df2),type="l",xlab="F",axes=FALSE)
axis(1)
axis(2)
abline(h=0)
abline(v=F,lwd=3)
mtext("F",side=3,line=1,at=F)
abline(v=F.krit,col="red",lwd=3)
mtext("F.krit",side=3,line=0,at=F.krit,col="red")


###################################################
### chunk number 29: 
###################################################
x <- seq(0,max(F,F.krit)*1.5,length=100)
plot(x,df(x,df1,df2),type="l",xlab="F",axes=FALSE)
axis(1)
axis(2)
abline(h=0)
abline(v=F,lwd=3)
mtext("F",side=3,line=1,at=F)
abline(v=F.krit,col="red",lwd=3)
mtext("F.krit",side=3,line=0,at=F.krit,col="red")



###################################################
### chunk number 30: 
###################################################
ord <- c("moski","zenski")
(xbar <- as.vector(xbar[ord]))
(s <- as.vector(s[ord]))
(n <- as.vector(n[ord]))



###################################################
### chunk number 31: 
###################################################
alpha <- 0.01
delta <- 0
(df <- n[1]+n[2]-2)
(t.krit <- qt(1-alpha,df))


###################################################
### chunk number 32: 
###################################################
xbar[1]-xbar[2]
s2 <- ((n[1]-1)*s[1]^2+(n[2]-1)*s[2]^2)/(n[1]+n[2]-2)
(t <- (xbar[1]-xbar[2]-delta)/sqrt(s2)*sqrt(n[1]*n[2]/(n[1]+n[2])))
(p <- 1-pt(t,df))
if(t<t.krit) cat("Povpreèje1 NI statistièno znaèilno veèje (p =",round(p,3),").\n") else
cat("Povpreèje1 JE  statistièno zanèilno veèje (p <",alpha,") (p =",round(p,3),").\n")



###################################################
### chunk number 33: critvist
###################################################
x <- seq(-max(t,t.krit)+1,max(t,t.krit)+1,length=100)
plot(x,dt(x,df),type="l",xlab="t",axes=FALSE)
axis(1)
axis(2)
abline(h=0)
abline(v=t,lwd=3)
mtext("t",side=3,line=1,at=t)
abline(v=t.krit,col="red",lwd=3)
mtext("t.krit",side=3,line=0,at=t.krit,col="red")


###################################################
### chunk number 34: 
###################################################
y <- masa
boxplot(y~spol, col=c("pink","lightblue"),ylab="masa [kg]")
rug(y[spol=="zenski"],side=2,col="pink",lwd=2)
rug(y[!spol=="zenski"],side=4,,col="lightblue",lwd=2)


###################################################
### chunk number 35: 
###################################################
(n <- tapply(y,spol,length))
(xbar <- tapply(y,spol,mean))
(s <- tapply(y,spol,sd))


###################################################
### chunk number 36: 
###################################################
alpha <- 0.01
(v <- sort(s^2))
ns <- as.vector(n[order(s)]) # da se znebim imen
(F <- as.vector(v[2]/v[1]))
(df1 <-ns[2]-1)
(df2 <-ns[1]-1)
(F.krit <- qf(1-alpha,df1,df2))
(p <- 1-pf(F,df1,df2))
if(F<F.krit) cat("Varianci NISTA statistièno znaèilno razlièni (p =",round(p,3),").\n") else
cat("Varianci STA  statistièno zanèilno razlièni (p <",alpha,") (p =",round(p,3),").\n")


###################################################
### chunk number 37: crittezaF
###################################################
x <- seq(0,max(F,F.krit)*1.5,length=100)
plot(x,df(x,df1,df2),type="l",xlab="F",axes=FALSE)
axis(1)
axis(2)
abline(h=0)
abline(v=F,lwd=3)
mtext("F",side=3,line=1,at=F)
abline(v=F.krit,col="red",lwd=3)
mtext("F.krit",side=3,line=0,at=F.krit,col="red")


###################################################
### chunk number 38: 
###################################################
x <- seq(0,max(F,F.krit)*1.5,length=100)
plot(x,df(x,df1,df2),type="l",xlab="F",axes=FALSE)
axis(1)
axis(2)
abline(h=0)
abline(v=F,lwd=3)
mtext("F",side=3,line=1,at=F)
abline(v=F.krit,col="red",lwd=3)
mtext("F.krit",side=3,line=0,at=F.krit,col="red")



###################################################
### chunk number 39: 
###################################################
ord <- c("moski","zenski")
(xbar <- as.vector(xbar[ord]))
(s <- as.vector(s[ord]))
(n <- as.vector(n[ord]))



###################################################
### chunk number 40: 
###################################################
alpha <- 0.01
delta <- 0
(df <- n[1]+n[2]-2)
(t.krit <- qt(1-alpha,df))


###################################################
### chunk number 41: 
###################################################
xbar[1]-xbar[2]
s2 <- ((n[1]-1)*s[1]^2+(n[2]-1)*s[2]^2)/(n[1]+n[2]-2)
(t <- (xbar[1]-xbar[2]-delta)/sqrt(s2)*sqrt(n[1]*n[2]/(n[1]+n[2])))
(p <- 1-pt(t,df))
if(t<t.krit) cat("Povpreèje1 NI statistièno znaèilno veèje (p =",round(p,3),").\n") else
cat("Povpreèje1 JE  statistièno zanèilno veèje (p <",alpha,") (p =",round(p,3),").\n")



###################################################
### chunk number 42: 
###################################################
x <- seq(-max(t,t.krit)+1,max(t,t.krit)+1,length=100)
plot(x,dt(x,df),type="l",xlab="t",axes=FALSE)
axis(1)
axis(2)
abline(h=0)
abline(v=t,lwd=3)
mtext("t",side=3,line=1,at=t)
abline(v=t.krit,col="red",lwd=3)
mtext("t.krit",side=3,line=0,at=t.krit,col="red")


###################################################
### chunk number 43: 
###################################################
set.seed(1234)
par(mfrow=c(2,1),mar=c(4,5,0,0))
varname <- "visina"
grp <- (spol)
y<- jitter(get(varname))
cols <- c("red","blue")[grp]
plot(y,spol,col=cols,cex=as.numeric(spol),xlab=varname)
rug(y[grp=="zenski"],0.6,1,col="red")
rug(y[!grp=="zenski"],0.6,3,col="blue")
plot(y,rank(y),col=cols,cex=as.numeric(spol),xlab=varname)
xr <-quantile(y,0.99)
text(xr,35,sum(rank(y)[grp=="zenski"]),col="red",adj=1)
text(xr,15,sum(rank(y)[grp=="moski"]),col="blue",adj=1)
text(xr,25,paste("povp=",sum(rank(y))/2),adj=1)
smpl <- sample(1:length(y),table(grp)["moski"])
rnd <- sum(smpl)
points(y[order(y)][smpl],smpl,pch=16,cex=0.75,col="darkgreen")
text(xr,5,paste("rnd=",rnd),col="darkgreen",adj=1)


###################################################
### chunk number 44: 
###################################################
set.seed(1234)
par(mfrow=c(2,1),mar=c(4,5,0,0))
varname <- "visina"
grp <- sample(spol)
y<- jitter(get(varname))
cols <- c("red","blue")[grp]
plot(y,spol,col=cols,cex=as.numeric(spol),xlab=varname)
rug(y[grp=="zenski"],0.6,1,col="red")
rug(y[!grp=="zenski"],0.6,3,col="blue")
plot(y,rank(y),col=cols,cex=as.numeric(spol),xlab=varname)
xr <-quantile(y,0.99)
text(xr,35,sum(rank(y)[grp=="zenski"]),col="red",adj=1)
text(xr,15,sum(rank(y)[grp=="moski"]),col="blue",adj=1)
text(xr,25,paste("povp=",sum(rank(y))/2),adj=1)
smpl <- sample(1:length(y),table(grp)["moski"])
rnd <- sum(smpl)
points(y[order(y)][smpl],smpl,pch=16,cex=0.75,col="darkgreen")
text(xr,5,paste("rnd=",rnd),col="darkgreen",adj=1)


###################################################
### chunk number 45: 
###################################################
wilcox.test(jitter(visina)~spol)


###################################################
### chunk number 46: 
###################################################
f <- table(lasje,oci)
f
addmargins(f)


###################################################
### chunk number 47: 
###################################################
e <- outer(rowSums(f),colSums(f))/sum(f)
addmargins(e)



###################################################
### chunk number 48: 
###################################################
f-e
(f-e)^2/e
1.96^2


###################################################
### chunk number 49: 
###################################################
alpha <- 0.05
(df <- (ncol(f)-1)*(nrow(f)-1))
(X2.krit <- qchisq(1-alpha,df))
(X2 <- sum((f-e)^2/e))
(p <- 1-pchisq(X2,df))
if(X2<X2.krit) cat("Spremenljivki NISTA odvisni (p =",round(p,4),").\n") else
cat("Spremenljivki STA odvisni (p <",alpha,") (p =",round(p,4),").\n")


###################################################
### chunk number 50: 
###################################################
chisq.test(lasje,oci,correct=FALSE)


###################################################
### chunk number 51: 
###################################################
f <- table(lasje,oci)
addmargins(f)


###################################################
### chunk number 52: 
###################################################
sum(f)*det(matrix(f,2,2))^2/prod(colSums(f),rowSums(f))



###################################################
### chunk number 53: 
###################################################
x <- seq(0+1,max(X2,X2.krit)*1.2,length=100)
plot(x,dchisq(x,df),type="l",xlab="t",axes=FALSE)
axis(1)
axis(2)
abline(h=0)
abline(v=X2,lwd=3)
mtext("X2",side=3,line=1,at=X2)
abline(v=X2.krit,col="red",lwd=3)
mtext("X2.krit",side=3,line=0,at=X2.krit,col="red")


###################################################
### chunk number 54: 
###################################################
plot(visina, roke,col=spol)
abline(0,1,col="darkgreen",lwd=2)


###################################################
### chunk number 55: 
###################################################
select <- roke>0
x <- visina[select]
y <- roke[select]
spol <- spol[select]
n <- length(x)



###################################################
### chunk number 56: 
###################################################
(xbar <- mean(x))
(ybar <- mean(y))
sum((x-xbar)*(y-ybar))/(n-1)
cov(x,y)
(r <- cov(x,y)/(sd(x)*sd(y)))
r^2
(b <- cov(x,y)/var(x))
(a <- mean(y)-b*mean(x))


###################################################
### chunk number 57: 
###################################################
plot(x, y,col=spol,xlab="visina",ylab="roke")
abline(0,1,col="darkgreen",lwd=2)
abline(c(a,b),col="red",lwd=4)
abline(h=mean(y),lty=3)
abline(v=mean(x),lty=3)
#points(mean(x),mean(y),cex=2,col=3,pch=16)


###################################################
### chunk number 58: 
###################################################
cor(x,y)
lsfit(x,y)$coefficients
lm(y~x)$coefficients
lm(y~x*spol)


###################################################
### chunk number 59: 
###################################################
plot(x,y,col=spol,xlab="visina",ylab="teza")
abline(0,1,col="darkgreen",lwd=2)
abline(c(a,b),col="red",lwd=4)
abline(lm(y~x),col="blue",lwd=1)
#abline(h=mean(y),lty=3)
#abline(v=mean(x),lty=3)
#points(mean(x),mean(y),cex=2,col=3,pch=16)
points(x,predict(lm(y~0+x*spol)),col=spol,pch=16)


###################################################
### chunk number 60: 
###################################################
anova(lm(roke~visina*spol))


###################################################
### chunk number 61: 
###################################################
x <- mati
y <- visina
plot(x,y,col=spol,xlab="mati",ylab="visina")
abline(0,1,col="darkgreen",lwd=2)
#abline(h=mean(y),lty=3)
#abline(v=mean(x),lty=3)
#points(mean(x),mean(y),cex=2,col=3,pch=16)
points(x,predict(lm(y~0+x*spol)),col=spol,pch=16)


###################################################
### chunk number 62: 
###################################################
x <- oce
y <- visina
plot(x,y,col=spol,xlab="oce",ylab="visina")
abline(0,1,col="darkgreen",lwd=2)
#abline(h=mean(y),lty=3)
#abline(v=mean(x),lty=3)
#points(mean(x),mean(y),cex=2,col=3,pch=16)
points(x,predict(lm(y~0+x*spol)),col=spol,pch=16)


###################################################
### chunk number 63: 
###################################################
x <- oce
y <- mati
plot(x,y,col=spol,xlab="oce",ylab="mati")
abline(0,1,col="darkgreen",lwd=2)
#abline(h=mean(y),lty=3)
#abline(v=mean(x),lty=3)
#points(mean(x),mean(y),cex=2,col=3,pch=16)
points(x,predict(lm(y~x)),col=spol,pch=16)



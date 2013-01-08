###################################################
### chunk number 1: 
###################################################
options(width=70)
#library(Hmisc)
#library(xlsReadWrite)


###################################################
### chunk number 2: 
###################################################
library(mvtnorm)


###################################################
### chunk number 3: 
###################################################
n <- 2
dx <-.2
mu <- rep(0,n)
sigma <- diag(n)
sigma <- matrix(c(1,-.3,-.3,4),ncol=n)
x1 <- seq(-5,5,dx)
x2 <- seq(-5,5,dx)
x <- expand.grid(x1=x1,x2=x2)
z <- matrix(dmvnorm(x,mu,sigma),nrow=length(x1),ncol=length(x2))
#contour(x1,x2,z)
for(i in seq(-30,30,5)) persp(x1,x2,z,phi=30,theta=i)


###################################################
### chunk number 4: 
###################################################
dx=.53321
x1 <- seq(-5,5,dx)
x2 <- seq(-5,5,dx)
X <- expand.grid(x=x1,y=x2)
z=with(X,matrix(1/(x^2+y^2),nrow=length(x1),ncol=length(x2)))
#contour(x1,x2,z)
persp(x1,x2,z,phi=30,theta=45,col="lightblue",shade=1)


###################################################
### chunk number 5: 
###################################################
dx=.53
x1 <- seq(-5,5,dx)
x2 <- seq(-5,5,dx)
X <- expand.grid(x=x1,y=x2)
z=with(X,matrix(x^3+y^2,nrow=length(x1),ncol=length(x2)))
contour(x1,x2,z)


###################################################
### chunk number 6: 
###################################################
#for(i in seq(-30,340,.5))
persp(x1,x2,z,phi=30,theta=340,col="lightblue",shade=1)


###################################################
### chunk number 7: 
###################################################
library(fields) ## za risanje brvnih plastnic


###################################################
### chunk number 8: 
###################################################

dx=1
x1 <- seq(200,600,dx)
x2 <- seq(200,600,dx)
X <- expand.grid(x=x1,y=x2)
pari <- with(X,which((x^2-y^2)==2008))
pari <- X[pari,]
z <- with(X,matrix(x^2-y^2,nrow=length(x1),ncol=length(x2)))
contour(x1,x2,z)
points(pari,col="red",pch=16)


###################################################
### chunk number 9: 
###################################################
dx=20
x1 <- seq(200,600,dx)
x2 <- seq(200,600,dx)
X <- expand.grid(x=x1,y=x2)
z <- with(X,matrix(x^2-y^2,nrow=length(x1),ncol=length(x2)))
pm <- drape.plot(x1,x2,z,phi=30,theta=-60,shade=.1,
col=terrain.colors(128))
pushpin( pari[,1],pari[,2],2008,pm,
text=apply(pari,1,paste,collapse=" , "),
col="red",height=.1,cex=1)




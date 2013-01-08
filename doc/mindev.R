###################################################
### chunk number 1: 
###################################################
options(width=70)
set.seed(1234)
library(Hmisc)
#library(xlsReadWrite)


###################################################
### chunk number 2: gmean
###################################################
gmean <- function(x,na.rm=TRUE){
#
# geometric mean
#
x <- x[!is.na(x)]
prod(x)^(1/length(x))
}
gmean(c(1,10,100,NA))


###################################################
### chunk number 3: showMinDeviation
###################################################
showMinDeviation <-
function(x=rnorm(10, 5, 2),a=seq(0, 20, .2),Xs=seq(0, max(a), .1),xlim=range(a),ylim=c(0,5),what=c("median", "mean", "gmean"),cols=c("black","blue","red")){
#
# dynamic plot of sum of deviations minima
#
#
n <- length(x)
maxa <- max(a)
basey <- max(ylim)/15
vabs <- Vectorize (FUN = function(x, a) sum(abs(x - a))/length (x), "a")
vkv <- Vectorize (FUN = function(x, a) sum((x - a)^2)/length (x), "a")
pkvoc <- Vectorize (FUN = function(x, a) prod(exp(log(x/a)^2))^(1/length(x)), "a")
#
par (mar = c(5, 4, 1, 4))
for (X in Xs) {
    x[n] <- X
    plot(a, a, ylab = "", xlim = xlim, ylim = ylim, type = "n")
    rug(x)
    rug(x[n], col = "red", lwd = 3)
    if (!is.na(pmatch ("median", what))) {
        ## median
        col <- cols[1]
        axis(1)
        mtext(expression(sum(abs(x - a)/n)), 2, 2)
        points(a, vabs(x, a), col = col)
        abline (v = median(x),col=col)
        MINa <- vabs(x, a = median(x))
        abline (h = MINa,col=col)
        text(0, MINa + .2, round(MINa, 2), xpd = TRUE, adj = 0,col=col)
        text(median(x), min(ylim)-basey, "Me", xpd = TRUE,col=col)
    }
    if (!is.na(pmatch ("mean", what))) {
        ## arithmetic mean
        col <- cols[2]
        axis(4, col = col, at = axTicks(2), label = axTicks(2) * 10, col.axis = col)
        mtext(expression(sum((x - a)^2)/n), 4, 2, col = col)
        points(a, vkv(x, a)/10, col = col)
        abline (v = mean(x), col = col)
        MINk <- vkv(x, a = mean(x))
        abline (h = MINk/10, col = col)
        text(maxa, MINk/10 + .2, round(MINk, 2), xpd = TRUE, adj = 1, col = col)
        text(mean(x), min(ylim)- basey*1.5, expression(bar (x)), xpd = TRUE, col = col)
    }
    if (!is.na(pmatch ("gmean", what))) {
        ## geometric mean
        col <- cols[3]
        points(a, pkvoc(x, a), col = col)
        abline (v = gmean(x), col = col)
        MINp <- pkvoc(x, a = gmean(x))
        abline (h = MINp, col = col)
        text(gmean(x), min(ylim)- basey*2, "G", xpd = TRUE, col = col)
        text(0, MINp + .2, round(MINp, 2), xpd = TRUE, adj = 0, col = col)
        mtext(expression((prod(e^{log(x/a)^2}))^{1/n}), 2, 2, col = col,adj=.8)
    }
}
}
showMinDeviation()


###################################################
### chunk number 4: 
###################################################
showMinDeviation(what=c("median"),Xs=20)


###################################################
### chunk number 5: 
###################################################
showMinDeviation(what=c("median","mean"),Xs=20)


###################################################
### chunk number 6: 
###################################################
showMinDeviation(Xs=20)


###################################################
### chunk number 7:  eval=FALSE
###################################################
## showMinDeviation()


###################################################
### chunk number 8: 
###################################################
gmean <- function(x,na.rm=TRUE){
#
# geometric mean
#
x <- x[!is.na(x)]
prod(x)^(1/length(x))
}
gmean(c(1,10,100,NA))


###################################################
### chunk number 9:  eval=FALSE
###################################################
## showMinDeviation <-
## function(x=rnorm(10, 5, 2),a=seq(0, 20, .2),Xs=seq(0, max(a), .1),xlim=range(a),ylim=c(0,5),what=c("median", "mean", "gmean"),cols=c("black","blue","red")){
## #
## # dynamic plot of sum of deviations minima
## #
## #
## n <- length(x)
## maxa <- max(a)
## basey <- max(ylim)/15
## vabs <- Vectorize (FUN = function(x, a) sum(abs(x - a))/length (x), "a")
## vkv <- Vectorize (FUN = function(x, a) sum((x - a)^2)/length (x), "a")
## pkvoc <- Vectorize (FUN = function(x, a) prod(exp(log(x/a)^2))^(1/length(x)), "a")
## #
## par (mar = c(5, 4, 1, 4))
## for (X in Xs) {
##     x[n] <- X
##     plot(a, a, ylab = "", xlim = xlim, ylim = ylim, type = "n")
##     rug(x)
##     rug(x[n], col = "red", lwd = 3)
##     if (!is.na(pmatch ("median", what))) {
##         ## median
##         col <- cols[1]
##         axis(1)
##         mtext(expression(sum(abs(x - a)/n)), 2, 2)
##         points(a, vabs(x, a), col = col)
##         abline (v = median(x),col=col)
##         MINa <- vabs(x, a = median(x))
##         abline (h = MINa,col=col)
##         text(0, MINa + .2, round(MINa, 2), xpd = TRUE, adj = 0,col=col)
##         text(median(x), min(ylim)-basey, "Me", xpd = TRUE,col=col)
##     }
##     if (!is.na(pmatch ("mean", what))) {
##         ## arithmetic mean
##         col <- cols[2]
##         axis(4, col = col, at = axTicks(2), label = axTicks(2) * 10, col.axis = col)
##         mtext(expression(sum((x - a)^2)/n), 4, 2, col = col)
##         points(a, vkv(x, a)/10, col = col)
##         abline (v = mean(x), col = col)
##         MINk <- vkv(x, a = mean(x))
##         abline (h = MINk/10, col = col)
##         text(maxa, MINk/10 + .2, round(MINk, 2), xpd = TRUE, adj = 1, col = col)
##         text(mean(x), min(ylim)- basey*1.5, expression(bar (x)), xpd = TRUE, col = col)
##     }
##     if (!is.na(pmatch ("gmean", what))) {
##         ## geometric mean
##         col <- cols[3]
##         points(a, pkvoc(x, a), col = col)
##         abline (v = gmean(x), col = col)
##         MINp <- pkvoc(x, a = gmean(x))
##         abline (h = MINp, col = col)
##         text(gmean(x), min(ylim)- basey*2, "G", xpd = TRUE, col = col)
##         text(0, MINp + .2, round(MINp, 2), xpd = TRUE, adj = 0, col = col)
##         mtext(expression((prod(e^{log(x/a)^2}))^{1/n}), 2, 2, col = col,adj=.8)
##     }
## }
## }
## showMinDeviation()


###################################################
### chunk number 10: minme
###################################################
set.seed(1221)
maxx <- 20
n <- 10
a <- seq(0,maxx,0.01)
x <- runif(n)*maxx
par(mar=c(5,4,1,3))
ylab <- expression(n(x <= a) - n(x > a))
plot(range(x),c(-n,n),type="n",xlab="",ylab=ylab,axes=FALSE)
axis(2)
segments(x,-.5,x,.5,lwd=2)
nvec <- Vectorize(FUN=function(x,a) {sum(x<=a)-sum(x>a)},"a")
vabs <- Vectorize (FUN = function(x, a) sum(abs(x - a))/length (x), "a")
points(a,nvec(x,a))
abline(h=0)
abline(v=median(x))
text(median(x),-n-1.5,"Me",xpd=TRUE)
lines(a,vabs(x,a))
labcurve(list(list(x=a,y=vabs(x,a))),labels=expression(sum(abs(x-a))),type="p")



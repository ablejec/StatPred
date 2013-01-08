###################################################
### chunk number 1: Author:
###################################################
###############################################
##                                           ##
## (c) Andrej Blejec (andrej.blejec@nib.si)  ##
##                                           ##
###############################################


###################################################
### chunk number 2: initialize
###################################################
options(width=70)
library(Hmisc)
#library(xlsReadWrite)


###################################################
### chunk number 3: priprava podatkov str. 73
###################################################
y <- c(
 8, 7, 6, 5, 7,
 8, 9, 7, 9,12,
10,13,18,16,12,
15,13,14,17,13,
 7, 6, 5, 4, 3,
12, 5, 7, 6, 9,
 8, 7, 9, 8, 10)
 n <- 5
A <- rep(paste("A",1:7,sep=""),each=n)
data <- data.frame(A,id=rep(1:n,7),y)
reshape(data,idvar="A",timevar="id",direction="wide")


###################################################
### chunk number 4: vsote in povpreèja
###################################################
aggregate(y,by=list(A),sum)
aggregate(y,by=list(A),mean)


###################################################
### chunk number 5: 
###################################################
aovFit <- aov(y~A,data=data)
aovFit
aovTable <- summary(aovFit)
aovTable


###################################################
### chunk number 6: 
###################################################
par(mfrow=c(2,2))
plot(aovFit)


###################################################
### chunk number 7: aov table structure
###################################################
str(aovTable[[1]])


###################################################
### chunk number 8: se2
###################################################
(se2 <- aovTable[[1]]$'Mean Sq'[2])
df <- (aovTable[[1]])$"Df"
(n <- df[2]/(df[1]+1)+1)


###################################################
### chunk number 9: 
###################################################
means <- model.tables(aovFit,"means",se=TRUE)
means


###################################################
### chunk number 10: 
###################################################
sqrt(2*se2/n)


###################################################
### chunk number 11: effects
###################################################
ucinek <- means$tables$A-means$tables$'Grand mean'
ucinek


###################################################
### chunk number 12: se(ucinkov)
###################################################
sqrt(se2/n)


###################################################
### chunk number 13: 
###################################################
effects <- model.tables(aovFit,"effects",se=TRUE)
effects


###################################################
### chunk number 14: Tukey HSD
###################################################
TukeyHSD(aovFit, ordered = TRUE)


###################################################
### chunk number 15: TukeyHSD
###################################################
plot(TukeyHSD(aovFit,ordered=TRUE),las=1)


###################################################
### chunk number 16: linearni model
###################################################
lmFit <- lm(y~0+A,data=data)
lmFit


###################################################
### chunk number 17: linearni model
###################################################
lmFit <- lm(y-mean(y)~0+A,data=data)
lmFit


###################################################
### chunk number 18: 
###################################################
summary(lmFit)


###################################################
### chunk number 19: linearni model
###################################################
lmFit2 <- lm(y~0+A,data=data,contrasts=contrasts(as.factor(A)))
lmFit2


###################################################
### chunk number 20: 
###################################################
summary(lmFit2)


###################################################
### chunk number 21: sessionInfo
###################################################
cat(win.version(),"\n")
toLatex(sessionInfo())
cat("Project path:\\verb'",dirname(getwd()),"'\n")


###################################################
### chunk number 22: 
###################################################
mainFile <- commandArgs(trailingOnly = TRUE)
mainFile <- strsplit(mainFile,'.',fixed=TRUE)[[1]][1]
projectName <- rev((strsplit(dirname(getwd()), "/"))[[1]])[1]
cat('> projectName <-"',projectName,'"; ',sep="")
cat(' mainFile <-"',mainFile,'"',sep="")


###################################################
### chunk number 23: vignette eval=FALSE
###################################################
## commandArgs()
## library(tkWidgets)
## # getrootpath <- function() {
## # fp <- (strsplit(getwd(), "/"))[[1]]
## # file <- file.path(paste(fp[-length(fp)], collapse = "/"))
## # return(file)
## # }
## # fileName <- function(name="bla",ext="PDF") paste(name,ext,sep=".")
##  openPDF(file.path(dirname(getwd()),"doc",paste(mainFile,"PDF",sep=".")))
##  viewVignette("viewVignette", projectName, file.path("../doc",paste(mainFile,"RNW",sep=".")))
## 



library("maxLik")
tunacalib <- read.csv("tunacalib.csv")
head(tunacalib)
summary(tunacalib)
counts <- table(tunacalib$brand)
counts
barplot(counts, main="Distribution of Brands",xlab="Brand Id",col="goldenrod1")


# built-in
require(foreign)
require(nnet)

attach(tunacalib)
mnlr <- multinom(brand ~ p1 + f1 + d1 + p2 + f2 + d2 + p3 + f3 + d3 + p4 + f4 + d4 + p5 + f5 + d5)
summary(mnlr)

z <- summary(mnlr)$coefficients/summary(mnlr)$standard.errors
z


# alt
library(mlogit)
tuna2 <- mlogit.data(tunacalib_mltn, shape = "wide", varying = 4:18, choice = "brand")
mnlr2 <- mlogit(brand ~ p + f + d, tuna2)
summary(mnlr2)
mnlr_interaction <-  mlogit(brand ~ p + f + d + p*f + p*d, tuna2)
summary(mnlr_interaction)


# lag

library(dyn)
require(zoo)

lagpad <- function(x, k) {
  c(rep(NA, k), x)[1 : length(x)] 
}

reg.koy = dyn$mlogit(brand ~ p + f + d + lagpad(p,1)*f + lagpad(p,1)*d, data = tuna2, na.omit = TRUE)
summary(reg.koy)




# MLE

nbrands <- length(counts)
y <- tunacalib$brand
delta <- matrix(0.0,nrow=nrow(tunacalib),ncol=nbrands)

for (i in 1:nrow(tunacalib))
{
  col<-y[i]
  delta[i,col]<-1
}

xmat <- as.matrix(tunacalib[,4:18])
nparms <- 9
parms <- runif(nparms)

logLik <- function(parms, delta, xmat)
{
  v1 <- parms[5]*xmat[,1]+parms[6]*xmat[,2]+parms[7]*xmat[,3]+parms[8]*xmat[,1]*xmat[,2]+parms[9]*xmat[,1]*xmat[,3]
  v2 <- parms[1]+parms[5]*xmat[,4]+parms[6]*xmat[,5]+parms[7]*xmat[,6]+parms[8]*xmat[,4]*xmat[,5]+parms[9]*xmat[,4]*xmat[,6]
  v3 <- parms[2]+parms[5]*xmat[,7]+parms[6]*xmat[,8]+parms[7]*xmat[,9]+parms[8]*xmat[,7]*xmat[,8]+parms[9]*xmat[,7]*xmat[,9]
  v4 <- parms[3]+parms[5]*xmat[,10]+parms[6]*xmat[,11]+parms[7]*xmat[,12]+parms[8]*xmat[,10]*xmat[,11]+parms[9]*xmat[,10]*xmat[,12]
  v5 <- parms[4]+parms[5]*xmat[,13]+parms[6]*xmat[,14]+parms[7]*xmat[,15]+parms[8]*xmat[,13]*xmat[,14]+parms[9]*xmat[,13]*xmat[,15]
  lden <- log(exp(v1)+exp(v2)+exp(v3)+exp(v4)+exp(v5))
  return(sum(v1*delta[,1]+v2*delta[,2]+v3*delta[,3]+v4*delta[,4]+v5*delta[,5]-lden))  
  # why -ledn: ln((e^a)/b)= a - lnb
  # ensures that we are only using vi of the chosen brand
}

mlfit <- maxLik(logLik, start=c(Int2=parms[1],Int3=parms[2],Int4=parms[3],Int5=parms[4],
                                Price=parms[5],Feature=parms[6],Display=parms[7],PF=parms[8],PD=parms[9])
                , delta=delta, xmat=xmat)
print(summary(mlfit))









newparms <- mlfit$estimate
v1 <- as.matrix(tuna[4:6])%*%as.matrix(newparms[5:7])
v2 <- as.matrix(tuna[7:9])%*%as.matrix(newparms[5:7]) + newparms[1]
v3 <- as.matrix(tuna[10:12])%*%as.matrix(newparms[5:7]) + newparms[2]
v4 <- as.matrix(tuna[13:15])%*%as.matrix(newparms[5:7]) + newparms[3]
v5 <- as.matrix(tuna[16:18])%*%as.matrix(newparms[5:7]) + newparms[4]

choiceProb <- function(vi)
{
  pi <- exp(vi)/(exp(v1)+exp(v2)+exp(v3)+exp(v4)+exp(v5))
  return(pi)
}

P1 <- choiceProb(v1)
P2 <- choiceProb(v2)
P3 <- choiceProb(v3)
P4 <- choiceProb(v4)
P5 <- choiceProb(v5)
P <- cbind(P1,P2,P3,P4,P5)
Q <- t(rep(1,5)%*%t(rep(1,nrow(tuna)))) - P
PChosen <- (as.matrix(P)*as.matrix(delta))%*%rep(1,5)

Z <- cbind(tuna$p1,tuna$p2,tuna$p3,tuna$p4,tuna$p5)
ZChosen <- (as.matrix(Z)*as.matrix(delta))%*%rep(1,5)

PED <- ((t(Q*Z)%*%rep(1,nrow(tuna)))/nrow(tuna))*newparms[5]
XED <- ((t(P*Z)%*%rep(1,nrow(tuna)))/nrow(tuna))*newparms[5]*(-1)

E <- matrix(rep(0,25),nrow=5,ncol=5)
for(i in 1:5){
  for(j in 1:5){
    if(i==j){
      E[i,i] <- PED[i]
    } else {
      E[i,j] <- XED[i]
    }
  }
}

E


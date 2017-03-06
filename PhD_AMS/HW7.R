library("maxLik")
travel <- read.delim("./travel.dat", header = TRUE, sep = "")

head(travel)
summary(travel)

delta <- matrix(travel$Mode,nrow=210,ncol=4,byrow=TRUE)
y <- delta %*% as.matrix(c(1,2,3,4),nrow = 1, ncol = 4)
time <- matrix(travel$Time,nrow=210,ncol=4,byrow=TRUE)
invc <- matrix(travel$Invc,nrow=210,ncol=4,byrow=TRUE)
invt <- matrix(travel$Invt,nrow=210,ncol=4,byrow=TRUE)
hinc <- matrix(travel$Hinc,nrow=210,ncol=4,byrow=TRUE)[,1]

counts <- table(y)
counts
ntrans <- length(counts)

xmat <- as.matrix(cbind(time,invc,invt,hinc))
nparms <- 9
parms <- runif(nparms)

logLik <- function(parms, delta, xmat)
{
  v1 <- parms[4]*xmat[,1]+parms[5]*xmat[,5]+parms[6]*xmat[,9]
  v2 <- parms[1]+parms[4]*xmat[,2]+parms[5]*xmat[,6]+parms[6]*xmat[,10]+parms[7]*xmat[,13]
  v3 <- parms[2]+parms[4]*xmat[,3]+parms[5]*xmat[,7]+parms[6]*xmat[,11]+parms[8]*xmat[,13]
  v4 <- parms[3]+parms[4]*xmat[,4]+parms[5]*xmat[,8]+parms[6]*xmat[,12]+parms[9]*xmat[,13]
  lden <- log(exp(v1)+exp(v2)+exp(v3)+exp(v4))
  return(sum(v1*delta[,1]+v2*delta[,2]+v3*delta[,3]+v4*delta[,4]-lden))  
}

mle1 <- nlm(logLik, parms, xmat=xmat, delta = delta, hessian=TRUE)


mlfit <- maxLik(logLik, start=c(Int2=parms[1],Int3=parms[2],Int4=parms[3],Time=parms[4],
                                Invc=parms[5],Invt=parms[6],Hinc2=parms[7],Hinc3=parms[8],
                                Hinc4=parms[9]), delta=delta, xmat=xmat)
print(summary(mlfit))


# Nested Logit

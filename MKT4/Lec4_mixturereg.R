data <- read.csv("./Fall 2016/Marketing Models/mixturereg.csv")
y <- as.vector(data[,2]); hist(y)

summary(data)

id <- data[,1]; xmat <- as.matrix(cbind(1,data[,3]))

npar <- 6; parms = runif(npar)

logLik <- function(parms, y, xmat, id){
  wt <- exp(parms[6])/(1+exp(parms[6]))
  xbeta1 <- xmat%*%parms[1:2]; xbeta2 <- xmat%*%parms[3:4]
  ll <- 0.0
  
  # looping over each individual
  for(i in 1:max(id)){
    sxbeta1 <- subset(xbeta1, id==i); sxbeta2 <- subset(xbeta2, id==i)
    sy <- subset(y, id==i)
    # find the conditional log likelihood of individual i given each segment 
    sum1 <- sum(dnorm(sy,mean=sxbeta1), sd=exp(parms[5]), log=TRUE)
    sum2 <- sum(dnorm(sy,mean=sxbeta2), sd=exp(parms[5]), log=TRUE)
    
    # we need to compute log(wt*exp(sum1)+(1-wt)*exp(sum2))
    # Note that wt*exp(sum1) is the same as exp(log(wt))*exp(sum1) = exp(log(wt)+sum1)
    # we compute ws1 = log(wt)+sum1, below
    
    ws1 <- sum1+log(wt); ws2 <- sum2+log(1-wt)
    
    # we now need to compute log(exp(ws1)+exp(ws2)), where the exp function can lead to computational overflow
    # we therefore use the log-sum-exp trick to reduce the chance of numerical problems
    # in computing the log-likelihood. We first compute teh max of ws1 and ws2, and subtract this
    # from each of ws1 and ws2, before exponentiating these, and then add the maximum afterwords
    
    m <- max(ws1, ws2)
    ll <- ll+m+log(exp(ws1-m)+exp(ws2-m))  
  }
  # the function above returns the negative log likelihood
  print(-ll)
  return(-ll)
}

mle1 <- nlm(logLik, parms, y=y, xmat=xmat, id=id, hessian=T)

# calculating the Hessian to obtain stdev
mode = mle1$estimate  # output parameter estimates
mle1$hessian
SE = sqrt(diag(solve(mle1$hessian)))  # output parameter SEs
Tvalue = mode/SE  # output parameter T-values
ll = mle1$minimum  # -2*log-likelihood
A1 <- cbind(Estimate=mode, SE=SE, Tvalue=Tvalue, minus2ll=ll)
A1




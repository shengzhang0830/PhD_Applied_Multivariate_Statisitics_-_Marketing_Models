install.packages("maxLik")
# ,repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"),dependencies = TRUE)
install.packages("miscTools",repos=c("http://rstudio.org/_packages", "http://cran.rstudio.com"))
require("miscTools")
library("maxLik")

set.seed(154)

# generate 1000 random numbers that follow a normal distr with mean 1 and sd 1.5
x<-rnorm(1000,1,1.5)

hist(x)

# likelihood fucntion
llf<-function(param){
  mu<-param[1]
  sigma<-param[2]
  # dnorm returns a vector (1000*1) of the log density of Xs 
  ll<-dnorm(x,mean=mu,sd=sigma,log=TRUE)
  return(sum(ll))
}

# use the likelihood function
llf(c(1.2,0.7))
# llf(c(1.2,0.7)) > llf(c(1.3,0.6)) beacuse the true mean parameter is 1


mlfit<-maxLik(llf,start=c(mu=0.5,sigma=0.75))

print(summary(mlfit))
# gives mu = 0.98, sigma = 1.52...

mlfit$hessian
cov<-(solve(-hessian(mlfit)))
# cov is the inverse of -H

serrors<-sqrt(cov)


















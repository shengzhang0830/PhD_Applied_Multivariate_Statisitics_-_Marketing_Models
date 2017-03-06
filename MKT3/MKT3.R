library("maxLik")

ChoiceData <- read.csv("./Fall 2016/Marketing Models/ChoiceData.csv",header = TRUE)
summary (ChoiceData)

counts <- table(ChoiceData$Chosen)
barplot(counts, main="Distribution of Brands",xlab="Brand Id",col="goldenrod1")
nbrands <- length(counts)

counts

y <- ChoiceData$Chosen

delta <- matrix(0.0,nrow=nrow(ChoiceData),ncol=nbrands)
# Remember a matrix can only contain data of the same type, while a dataframe can contain
# data of different type.

# Create a Y matrix
for (i in 1:nrow(ChoiceData))
{
  col<-y[i]
  delta[i,col]<-1
}

# Create a matrix of X
xmat <- as.matrix(ChoiceData[,3:11])

npar <- 5

parms <- runif(5)
# help(runif)

logLik <- function(parms, delta, xmat)
{
  v1 <- parms[3]*xmat[,1]+parms[4]*xmat[,2]+parms[5]*xmat[,3]
  v2 <- parms[1]+parms[3]*xmat[,4]+parms[4]*xmat[,5]+parms[5]*xmat[,6]
  v3 <- parms[2]+parms[3]*xmat[,7]+parms[4]*xmat[,8]+parms[5]*xmat[,9]
  
  lden <- log(exp(v1)+exp(v2)+exp(v3))
  
  sum(v1*delta[,1]+v2*delta[,2]+v3*delta[,3]-lden)
}

mlfit <- maxLik(logLik, start=c(Int2=parms[1],Int3=parms[2],Price=parms[3],
                                Feature=parms[4],Display=parms[5]), delta=delta, xmat=xmat)

print(summary(mlfit))
# Gives the same ML paramenter estimates as Excel does, but also gives stderrs and t-values. 
# Int2 = 0.13 means that the brand preference for brand 2 is higher than that for brand 1.





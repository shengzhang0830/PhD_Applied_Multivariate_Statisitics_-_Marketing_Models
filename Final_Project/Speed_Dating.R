library(rstan)
library(rstudioapi)
library(coda)
library(ggmcmc)
library(loo)
rstan_options(auto_write=TRUE)
library(parallel)
options(mc.cores=detectCores()-2)

# Read in data
rDataF1<-read.csv("./Speed_Dating_SZ_resorted.csv", header=T)
rDataF <- na.omit(rDataF1)
N <- nrow(rDataF)
col <- ncol(rDataF)
max_code <- 552
p <- 6
nr <- 2

y <- rDataF[,3:4]
xi <- data.matrix(rDataF[,5:10])
xj <- data.matrix(cbind(rDataF[,5:7], rDataF[,11:13]))
code_i <- rDataF[,1]
code_j <- rDataF[,2]

# Create Stan input
stanDat <- list(N = N, J = max_code, p = p, nr = nr, rowj = code_i, colj = code_j, neqn = 2, xi = xi, xj = xj, y = y)


# Use Stan for estimation
sfit <- stan(file = "sheng1.stan", data=stanDat, iter=1000, chains = 2)

# Prepare for plotting
s <- mcmc.list(lapply(1:ncol(sfit), function(x) mcmc(as.array(sfit)[,x,])))
S <- ggs(s[,1:6])
                      
# Plot the results                      
ggs_histogram(S)
ggs_traceplot(S)
ggs_density(S)
ggs_compare_partial(S)
ggs_running(S)
ggs_autocorrelation(S)
                      
sink("sfit.txt")
print(sfit)
sink()

sink("Fit.txt")
Fit <- rstan::extract(sfit)
print(Fit)
sink()

# Show and save the estimated parameters                      
dfsfit <- as.data.frame(sfit)
print(colnames(dfsfit))

sink("parameters.txt")
summary(dfsfit[,1:6])
summary(dfsfit[,1111:1116])
sink()

# Investigate reciprocity effects                      
alpha <- colMeans(dfsfit[,7:558])
alpha
beta <- colMeans(dfsfit[,559:1110])
beta
plot(alpha, beta, main="Reciprocity", 
     xlab="Rater Variance", ylab="Target Variance", pch=19, col = "deepskyblue3")

cor(alpha,beta)



# Save the results
save(print(sfit), file="sfit.txt", ascii = T, list = character())
save(Fit, file="Fit.txt", ascii = T)


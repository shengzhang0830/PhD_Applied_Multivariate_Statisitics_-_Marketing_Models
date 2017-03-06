library(rstan)
library(rstudioapi)
library(coda)
library(ggmcmc)
library(loo)
rstan_options(auto_write=TRUE)
library(parallel)
options(mc.cores=detectCores()-2)

# Read in data
rDataF<-read.csv("PoissonData.csv", header=FALSE)
N <- nrow(rDataF)
col<-ncol(rDataF)
p <-col-1

y<-rDataF[,13]
X <-data.matrix(rDataF[1:12])

# Creating a holdout sample of size 10% of the data
Nout <- 0.1 *N 

# Randomly isolating Nout data points
keep <- sample(x = N,size = Nout)
y_hold <- y[keep]
X_hold <- X[keep,]

# Defining the estimation sample
N_est    <- 0.9 *N 
y_est <- y[-keep]
X_est <- X[-keep,]

# Create Stan input
stanDat <- list(N1 = N_est, xmat1=X_est, y1=y_est)

# Stan Code
code1 ="
data{
int<lower=0> N1;
matrix[N1,12] xmat1;
int<lower=0> y1[N1];
}

parameters{
vector[12] beta;
}

model{
vector[N1] lambda;

beta ~ cauchy(0,2.5);

for(i in 1:N1) lambda[i] = exp(dot_product(beta, xmat1[i]));
y1 ~ poisson(lambda);
}

generated quantities{
vector[N1] lambda;
for(i in 1:N1){
lambda[i] = exp(dot_product(beta, xmat1[i]));
}
}
"


# Use Stan for estimation
sfit <- stan(model_code = code1, data=stanDat, iter=400, chains = 2)
# file="HW3.stan"

print(sfit)

Fit <- rstan::extract(sfit)
# P <- colMeans(Fit$P)

lambda_in_sample <- colMeans(Fit$lambda)
lambda_in_sample

MAD_in_sample <- mean(abs(lambda_in_sample-y_est))
MAD_in_sample



# Hold-out Data Predictions
stanDat_hold <- list(N1 = N_est, N2 = Nout, xmat1 = X_est, xmat2=X_hold, y1=y_est)

code3 ="
data{
int<lower=0> N1;
int<lower=0> N2;
matrix[N1,12] xmat1;
matrix[N2,12] xmat2;
int<lower=0> y1[N1];
}

parameters{
vector[12] beta;
}

model{
vector[N1] lambda;

beta ~ cauchy(0,2.5);

for(i in 1:N1) lambda[i] = exp(dot_product(beta, xmat1[i]));
y1 ~ poisson(lambda);
}

generated quantities{
vector[N2] lambda;
for(i in 1:N2){
lambda[i] = exp(dot_product(beta, xmat2[i]));
}
}
"

sfit3 <- stan(model_code = code3,data = stanDat_hold,iter = 400, chains = 2)
print(sfit3)
Fit3 <- rstan::extract(sfit3)
lambda_hold <- colMeans(Fit3$lambda)
lambda_hold

MAD_hold <- mean(abs(lambda_hold-y))
MAD_hold










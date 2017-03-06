
Q4_23_Data <- c(-0.6, 3.1, 25.3, -16.8, -7.1, -6.2, 25.2, 22.6, 26)

qqnorm(Q4_23_Data)

ProbLv <- seq(0.5/9, 1, 1/9)
ProbLv

StdN_Quantiles <- qnorm(ProbLv)
StdN_Quantiles

mean(Q4_23_Data)
mean(StdN_Quantiles)
cor(Q4_23_Data,StdN_Quantiles)


Q4_26_x1 <- c(1,2,3,3,4,5,6,8,9,11)
Q4_26_x2 <- c(18.95, 19.00, 17.95, 15.54, 14.00, 12.95, 8.94, 7.49, 6.00, 3.99)
cov(Q4_26_x1, Q4_26_x2)
var(Q4_26_x1)

# x <- c(-4.2, 18.95-12.48)
# y <- matrix(c(2.19, 1.257, 1.257, 0.754),nrow=2,ncol=2)
# t(x)%*%y%*%x

dist <- c(1.875,2.020,2.901,0.735,0.311,0.018,3.733,0.817,1.375,4.215)
ProbLv2 <- seq(0.05, 1, 0.1)
Chi_Quantiles <- qchisq(ProbLv2,2)
ordered_dist <- sort(dist)
plot(Chi_Quantiles, ordered_dist)

# 5.1
a <- matrix(c(2,8,6,8,12,9,9,10),nrow=2,ncol=4, byrow = TRUE)
b <- matrix(c(0.75,-0.25,-0.25,-0.25,-0.25,0.75,-0.25,-0.25,-0.25,-0.25,0.75,-0.25,-0.25,-0.25,-0.25,0.75),nrow=4,ncol=4,byrow=TRUE)
(a%*%b%*%t(a))/3
s_inv <- solve((a%*%b%*%t(a))/3)
d<- matrix(c(-1,-1), nrow=1,ncol=2, byrow = TRUE)
4*(d%*%s_inv%*%t(d))
qf(0.99,2,2)

x <- c(0.564-0.55, 0.603-0.6)
y <- matrix(c(203.018, -163.391, -163.391, 200.228),nrow=2,ncol=2,byrow=TRUE)
42*t(x)%*%y%*%x

# 6.5
x_bar <- matrix(c(46.1, 57.3, 50.4),nrow=3,ncol=1,byrow=TRUE)
c <- matrix(c(1, -1, 0, 0, 1, -1),nrow=2,ncol=3,byrow=TRUE)
S <- matrix(c(101.3, 63.0, 71.0, 63.0, 80.2, 55.6, 71.0, 55.6, 97.4),nrow=3,ncol=3,byrow=TRUE)
40*t(c%*%x_bar)%*%solve(c%*%S%*%t(c))%*%(c%*%x_bar)
c1 <- matrix(c(1, -1, 0),nrow=1,ncol=3,byrow=TRUE)
S1 <- matrix(c(101.3, 63.0, 63.0, 80.2, 71.0, 55.6),nrow=2,ncol=2,byrow=TRUE)
c1%*%S%*%t(c1)
c2 <- matrix(c(0, 1, -1),nrow=1,ncol=3,byrow=TRUE)
c3 <- matrix(c(1, 0, -1),nrow=1,ncol=3,byrow=TRUE)
c2%*%S%*%t(c2)
c3%*%S%*%t(c3)
sqrt(56.7/40)*sqrt(6.67)

# 6.6
x <- matrix(c(3,1,2,3,6,3),nrow=3,ncol=2)
I_1 <- matrix(c(2/3,-1/3,-1/3,-1/3,2/3,-1/3,-1/3,-1/3,2/3),nrow=3,ncol=3)
(t(x)%*%I_1%*%x)/2
(7/12)*(76/5)

# 6.26
x1 <- matrix(c(0.153, -0.231, -0.322, -0.339), nrow=4, ncol=1)
x2 <- matrix(c(0.151, 0.180, 0.256, 0.257), nrow=4, ncol=1)
c <- matrix(c(-1, 1, 0, 0, 0, -1, 1, 0, 0, 0, -1, 1),nrow=3,ncol=4,byrow=TRUE)
spooled <- matrix(c(0.804, 0.355, 0.228, 0.232, 0.355, 0.722, 0.233, 0.199, 0.228, 0.233, 0.592, 0.239, 0.232, 0.199, 0.239, 0.479),nrow=4,ncol=4)
(58*28/86)*t(c%*%(x1-x2))%*%solve(c%*%spooled%*%t(c))%*%(c%*%(x1-x2))
qf(0.95,3,84)*3*84/82

# 6.27
data <- read.csv("./Fall 2016/Applied Multivariate Statistics/Applied Multivariate Statistics - HW4 - DataApplied Multivariate Statistics - HW4 - Data.csv")
mean <- sapply(data, mean)
cv <- cov(data, data)
spooled <- 1/2*(cv[1:4,1:4]+cv[5:8,5:8])
c <- matrix(c(-1, 1, 0, 0, 0, -1, 1, 0, 0, 0, -1, 1),nrow=3,ncol=4,byrow=TRUE)
diff <- mean[1:4]-mean[5:8]
30*t(c%*%diff)%*%solve(c%*%spooled%*%t(c))%*%(c%*%diff)
qf(0.95,3,57)*58*3/57
30*diff%*%solve(spooled)%*%diff
qf(0.95,1,58)

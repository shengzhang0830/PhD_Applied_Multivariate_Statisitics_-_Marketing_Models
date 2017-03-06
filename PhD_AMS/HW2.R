install.packages("installr")
updateR()

install.packages("ggplot2")
library("ggplot2")
install.packages("GGally")
library("GGally")

pm <- ggpairs(ams_hw2)
pm

pm <- plot(Applied.Multivariate.Statistics...HW2...Data[,1:7],diagonal="histogram", smooth = FALSE)


pm <- plotmatrix(Applied.Multivariate.Statistics...HW2...Data[,1:7])


sapply(ams_hw2,mean)
cov(ams_hw2, ams_hw2, use="all.obs")
cor(ams_hw2, ams_hw2, use="all.obs")



rm(Applied.Multivariate.Statistics...HW2...Data)

library(ggplot2)
library(rjags)
library(ggmcmc)

set.seed(123)

source("dataset.R")

plot(data$t,data$Y,type="l",col="red",xlim=c(5.5,7.1),main="Anti-HBs measurements for each infant",
     xlab="Time since final vaccination", ylab="Anti-HBs titre (mIU)")
points(data$t,data$Y,col="red",xlim=c(5.5,7.1),pch=0)
library(ggplot2)
library(rjags)
library(ggmcmc)

source("dataset.R")

N=106
J=3
t=data$t

sigma=0.8
tau=1/(sigma^2)
alpha=6
beta=1

Y=data.frame(matrix(ncol = J, nrow = N))

inits1 <- list(alpha0 = 4, beta0 = 0, gamma = 0, tau.alpha = 1, tau.beta = 1, tau = 1 )
inits3 <- list(alpha=4, beta = 0, tau = 1)

rip=30

set.seed(123)

dic1=rep(NA,rip)
dic3=rep(NA,rip)
dics=rep(NA,rip)

for(n in 1:rip){
  for( i in 1 : N ) {
    for( j in 1 : J ) {
      mu <- alpha + beta * (t[i,j] - 6.5)
      Y[i , j] = rnorm(n=1, mean=mu, sd=sigma)
    }
  }
  
  data3.eval = data
  data3.eval$Y = Y
  data3.eval$n=apply(!is.na(data3.eval$Y),1,sum)
  
  model1 = jags.model("modelHEP1.txt", data=data3.eval, inits=inits1, n.adapt=5000, n.chains=3)
  model3 = jags.model("modelHEP3.txt", data=data3.eval, inits=inits3, n.adapt=5000, n.chains=3)
  
  model1.dic = dic.samples(model1,20000)
  model3.dic = dic.samples(model3,20000)
  
  dic1[n] = sum(model1.dic$deviance)+sum(model1.dic$penalty)
  dic3[n] = sum(model3.dic$deviance)+sum(model3.dic$penalty)
  dics[n] = which.min(c(dic1[n], dic3[n]))
  
}

x=c(1:30)

plot (c(1,30),c(700,850),type="n", xlab="Iteration",ylab="DIC values",
      main="DIC comparison from data of Model 3")
lines(x,dic1,col="red",lwd=2.5)
points(dic1, col = "red",pch=16)
text(x, dic1, round(dic1, 2), cex=0.65,pos=2)
lines(x,dic3,col="blue",lwd=2.5)
points(dic3, col = "blue",pch=16)
legend("topright", lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"),
       legend= c("Model 1","Model 3"))
grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)
text(x, dic3, round(dic3, 2), cex=0.65,pos=4)

barplot(table(dics), xlab = "Model", ylab = "DIC count",
        main = "DIC counts - Data of Model 3", col = c('red',"blue"), 
        ylim = c(0,30))


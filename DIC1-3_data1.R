library(ggplot2)
library(rjags)
library(ggmcmc)

source("dataset.R")

N=106
J=3
t=data$t
y0=data$y0

tau=sqrt(1/0.7)
alpha_c=6
tau_alpha=sqrt(1/0.7)
beta_c=-1
tau_beta=sqrt(1/0.7)
gamma = 1

alpha = rnorm(n=N,alpha_c,tau_alpha)
beta = rnorm(n=N,beta_c,tau_beta)
Y=data.frame(matrix(ncol = J, nrow = N))

hep1.inits <- list(alpha0 = 4, beta0 = 0, gamma = 0, tau.alpha = 1, tau.beta = 1, tau = 1 )
hep3.inits <- list(alpha=4, beta = 0, tau = 1)

rip=30

set.seed(123)

dic1=rep(NA,rip)
dic3=rep(NA,rip)
dics=rep(NA,rip)


for(n in 1:rip){
  for(i in 1:N){
    for(j in 1:J){
      mu <- alpha[i] + beta[i] * (t[i,j] - 6.5) + gamma * (y0[i] - mean(y0[]))
      Y[i , j] = rnorm(n=1, mean=mu, sd=tau)  
    }
  }
  
  data1.eval = data
  data1.eval$Y = Y
  data1.eval$n=apply(!is.na(data1.eval$Y),1,sum)
  
  model1 = jags.model("modelHEP1.txt", data=data1.eval, inits=hep1.inits, n.adapt=5000, n.chains=3)
  model3 = jags.model("modelHEP3.txt", data=data1.eval, inits=hep3.inits, n.adapt=5000, n.chains=3)
  
  hep1.mod.dic = dic.samples(model1,20000)
  hep3.mod.dic = dic.samples(model3,20000)
  
  dic1[n] = sum(hep1.mod.dic$deviance)+sum(hep1.mod.dic$penalty)
  dic3[n] = sum(hep3.mod.dic$deviance)+sum(hep3.mod.dic$penalty)
  dics[n] = which.min(c(dic1[n], dic3[n]))
  
}

x=c(1:30)

plot (c(1,30),c(1000,1800),type="n", xlab="Iteration",ylab="DIC values",
      main="DIC comparison from data of Model 3")
lines(x,dic1,col="red",lwd=2.5)
points(dic1, col = "red",pch=16)
lines(x,dic3,col="blue",lwd=2.5)
points(dic3, col = "blue",pch=16)
legend("topright", lty=c(1,1), lwd=c(2.5,2.5),col=c("red","blue"),
       legend= c("Model 1","Model 3"))
grid(nx = NULL, ny = nx, col = "lightgray", lty = "dotted",
     lwd = par("lwd"), equilogs = TRUE)

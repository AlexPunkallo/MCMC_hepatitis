library(ggplot2)
library(rjags)
library(ggmcmc)

set.seed(123)

source("dataset.R")

N=106
J=3
t=data$t

sigma=0.8
tau=1/(sigma^2)
alpha=6
beta=1

Y=data.frame(matrix(ncol = J, nrow = N))

for( i in 1 : N ) {
  for( j in 1 : J ) {
    mu <- alpha + beta * (t[i,j] - 6.5)
    Y[i , j] = rnorm(n=1, mean=mu, sd=sigma)
  }
}

data3.eval = data
data3.eval$Y = Y
data3.eval$n=apply(!is.na(data3.eval$Y),1,sum)


hep3.eval.inits <- list(alpha=4, beta = 0, tau = 1)
hep3.eval.mod = jags.model("modelHEP3.txt", data=data3.eval, inits=hep3.eval.inits, n.adapt=5000, n.chains=3)
#update(sim.model, n.iter=5000)
hep3.eval.mod.samp = coda.samples(model=hep3.eval.mod,
                                  variable.names=c("alpha", "beta","tau"), 
                                  n.iter=50000,
                                  n.thin=100,
                                  n.burnin=5000)

summary(hep3.eval.mod.samp)

#          mean   sd     MC_error 
# alpha0  6.129  0.1586  0.004995 
# beta0  -1.075  0.1203  0.009152 
# gamma   1.082  0.2187  0.01668
# sigma   1.029  0.06513 0.003905


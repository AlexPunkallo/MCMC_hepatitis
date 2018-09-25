library(ggplot2)
library(rjags)
library(ggmcmc)

set.seed(123)

source("dataset.R")

N=106
J=3
t=data$t

tau=sqrt(1/0.8)
alpha_c=6
tau_alpha=sqrt(1/0.8)
beta_c=-1
tau_beta=sqrt(1/0.8)
gamma = 10
theta=2
psi=sqrt(1/0.8)

alpha = rnorm(n=N,alpha_c,tau_alpha)
beta = rnorm(n=N,beta_c,tau_beta)
mu0=rep(NA,N)
y0=rep(NA,N)

Y=data.frame(matrix(ncol = J, nrow = N))

for( i in 1 : N ) {
  mu0[i] = rnorm(n=1,theta,psi)
  y0[i] = rnorm(n=1,mu0[i],tau)
}

for( i in 1 : N ) {
  for( j in 1 : J ) {
    mu <- alpha[i] + beta[i] * (t[i,j] - 6.5) + gamma * (mu0[i] - mean(y0[]))
    Y[i , j] = rnorm(n=1, mean=mu, sd=tau)
  }
}

data2.eval = data
data2.eval$Y = Y
data2.eval$n=apply(!is.na(data2.eval$Y),1,sum)


hep2.eval.inits <- list(alpha0 = 4, beta0 = 0, gamma = 0, tau.alpha = 1, tau.beta = 1, tau = 1, theta = 6, psi = 1)
hep2.eval.mod = jags.model("modelHEP2.txt", data=data2.eval, inits=hep2.eval.inits, n.adapt=5000, n.chains=3)
#update(sim.model, n.iter=5000)
hep2.eval.mod.samp = coda.samples(model=hep2.eval.mod,
                                  variable.names=c("alpha0", "beta0","gamma","tau.alpha","tau.beta","tau","theta","psi"), 
                                  n.iter=50000,
                                  n.thin=100,
                                  n.burnin=5000)

summary(hep2.eval.mod.samp)

#          mean   sd     MC_error  # OPENBUGS OUTPUT
# alpha0  6.129  0.1586  0.004995 
# beta0  -1.075  0.1203  0.009152 
# gamma   1.082  0.2187  0.01668
# sigma   1.029  0.06513 0.003905

# Set the working directory

library(ggplot2)
library(rjags)
library(ggmcmc)

set.seed(123)

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

for( i in 1 : N ) {
  for( j in 1 : J ) {
    mu <- alpha[i] + beta[i] * (t[i,j] - 6.5) + gamma * (y0[i] - mean(y0[]))
    Y[i , j] = rnorm(n=1, mean=mu, sd=tau)
  }
}


data1.eval = data
data1.eval$Y = Y
data1.eval$n=apply(!is.na(data1.eval$Y),1,sum)


hep1.eval.inits <- list(alpha0 = 4, beta0 = 0, gamma = 0, tau.alpha = 1, tau.beta = 1, tau = 1 )
hep1.eval.sim = jags.model("modelHEP1.txt", data=data1.eval, inits=hep1.eval.inits, n.adapt=5000, n.chains=3)
#update(sim.model, n.iter=5000)
hep1.eval.mod.samp = coda.samples(model=hep1.eval.sim,
                              variable.names=c("alpha0", "beta0","gamma","tau.alpha","tau.beta","tau"), 
                              n.iter=50000,
                              n.thin=10,
                              n.burnin=5000)
summary(hep1.eval.mod.samp)

#         mean    sd       MC_error  # OPENBUGS
# alpha0  6.138   0.1503   0.001816 
# beta0  -1.064   0.1206   0.008653 
# gamma   0.6691  0.08149  0.00228 
# sigma   1.003   0.05471  0.001282 

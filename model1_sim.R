library(ggplot2)
library(rjags)
library(ggmcmc)

set.seed(123)

source("dataset.R")

hep1.inits <- list(alpha0 = 4, beta0 = 0, gamma = 0, tau.alpha = 1, tau.beta = 1, tau = 1 )
hep1.sim = jags.model("modelHEP1.txt", data=data, inits=hep1.inits, n.adapt=5000, n.chains=3)

#update(hep1.sim, n.iter=5000)

hep1.mod.samp = coda.samples(model=hep1.sim,
                              variable.names=c("alpha0", "beta0","gamma","tau.alpha","tau.beta","tau"), 
                              n.iter=50000,
                              n.thin=10,
                              n.burnin=5000)

summary(hep1.mod.samp)

hep1_results = ggs(hep1.mod.samp)

ggs_density(hep1_results)  # OPENBUGS: alpha0 = 6.138, beta0 = -1.064
ggs_traceplot(hep1_results)
ggs_running(hep1_results)
ggs_autocorrelation(hep1_results)
ggs_crosscorrelation(hep1_results)

  
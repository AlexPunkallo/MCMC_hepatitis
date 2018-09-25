library(ggplot2)
library(rjags)
library(ggmcmc)

source("dataset.R")

hep2.inits <- list(alpha0 = 4, beta0 = 0, gamma = 0, tau.alpha = 1, tau.beta = 1, tau = 1, theta = 6, psi = 1)
hep2.sim = jags.model("modelHEP2.txt", data=data, inits=hep2.inits, n.adapt=5000, n.chains=3)

#update(sim.model, n.iter=5000)

hep2.mod.samp = coda.samples(model=hep2.sim,
                              variable.names=c("alpha0", "beta0","gamma","tau.alpha","tau.beta","tau","theta","psi"), 
                              n.iter=50000,
                              n.thin=10,
                              n.burnin=5000)

summary(hep2.mod.samp)

hep2.results = ggs(hep2.mod.samp)

ggs_density(hep2.results)
ggs_traceplot(hep2.results)
ggs_running(hep2.results)
ggs_autocorrelation(hep2.results)
ggs_crosscorrelation(hep2.results)


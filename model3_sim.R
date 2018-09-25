library(ggplot2)
library(rjags)
library(ggmcmc)

source("dataset.R")

hep3.inits <- list(alpha=4, beta = 0, tau = 1)
hep3.sim = jags.model("modelHEP3.txt", data=data, inits=hep3.inits, n.adapt=5000, n.chains=3)

#update(sim.model, n.iter=5000)

hep3.mod.samp = coda.samples(model=hep3.sim,
                             variable.names=c("alpha", "beta","tau"), 
                             n.iter=50000,
                             n.thin=100,
                             n.burnin=5000)

summary(hep3.mod.samp)

hep3.results = ggs(hep3.mod.samp)

ggs_density(hep3.results)
ggs_traceplot(hep3.results)
ggs_running(hep3.results)
ggs_autocorrelation(hep3.results)
ggs_crosscorrelation(hep3.results)


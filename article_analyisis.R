load("data_Tenan_et_al_MEE_2014.Rdata")


inits <- function() {
    list(
        alpha.phi = runif(1, -2, 2),
        beta.time.phi = runif(1, -2, 2),
        alpha.rho = runif(1, -0.5, 0.5),
        sigma.rho = runif(1, 0, 1),
        alpha.p = runif(1, -0.5, 0.5),
        mu = runif(bugs.data.storm$s, 0.3, 1)
    )
}
# Define  parameters  to be  monitored
parameters <- c(
    "phi",
    "alpha.phi",
    "mean.phi",
    "beta.time.phi",
    "gamma",
    "rho",
    "alpha.rho",
    "mean.rho",
    "sigma.rho",
    "p",
    "alpha.p",
    "mean.p"
)
# MCMC  settings
niter <- 100000
nthin <- 10
nburn <- 30000
nchains <- 3
# Call  JAGS
library("R2jags")
out <- jags(bugs.data.storm, inits, parameters, "model.jags", n.chains = nchains, n.thin = nthin, n.iter = niter, n.burnin = nburn, working.directory = getwd())
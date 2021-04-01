load("data_Tenan_et_al_MEE_2014.Rdata")


inits <- function() {
    list(
        alpha.phi = runif(1, -2, 2),
        beta.time.phi = runif(1, -2, 2),
        alpha.f = runif(1, -0.5, 0.5),
        sigma.f = runif(1, 0, 1),
        alpha.p = runif(1, -0.5, 0.5),
        mu = runif(bugs.data.scopoli$s, 0.3, 1)
    )
}
# Define  parameters  to be  monitored
parameters_f <- c(
    "phi",
    "alpha.phi",
    "mean.phi",
    "beta.time.phi",
    "gamma",
    "f",
    "alpha.f",
    "mean.f",
    "sigma.f",
    "p",
    "alpha.p",
    "mean.p",
    "rho"
)

parameters_rho <- c(
    "phi",
    "alpha.phi",
    "mean.phi",
    "beta.time.phi",
    "gamma",
    "f",
    "alpha.rho",
    "mean.rho",
    "sigma.rho",
    "p",
    "alpha.p",
    "mean.p",
    "rho"
)


# MCMC  settings
niter <- 100000
nthin <- 10
nburn <- 30000
nchains <- 3
# Call  JAGS
library("R2jags")
out <- jags(bugs.data.scopoli, inits, parameters_f, "model.jags", n.chains = nchains, n.thin = nthin, n.iter = niter, n.burnin = nburn, working.directory = getwd())


###  derive  data  for  the  model
# e = index  of the  earliest  observation
get.first <- function(x) min(which(x != 0))
e <- apply(CH, 1, get.first)
# l = index  of the  last  observation
get.last <- function(x) max(which(x != 0))
l <- apply(CH, 1, get.last)
# u = number  of  animals  observed  for the  first time at i
u <- as.data.frame(table(e))$Freq
# n = number  of  animals  observed  at i
n <- colSums(CH)
# v = number  of  animals  observed  for the  last  time at i
v <- as.data.frame(table(l))$Freq
# d = number  of  animals  removed  from  the  population  at time i
d <- rep(0, dim(CH)[2])
# Time  covariate
time <- c(1:(dim(CH)[2] - 1))
# standardize  time
stand_time <- (time - mean(time)) / sd(time)
# Bundle  data
bugs.data <- list(u = u, n = n, v = v, d = d, s = dim(CH)[2], TIME = stand_time)
##  Initial  values
inits <- function() {
    list(
        alpha.phi = runif(1, -2, 2),
        beta.time.phi = runif(1, -2, 2),
        alpha.f = runif(1, -0.5, 0.5),
        sigma.f = runif(1, 0, 1),
        alpha.p = runif(1, -0.5, 0.5),
        mu = runif(dim(CH)[2], 0.3, 1)
    )
}
# Define  parameters  to be  monitored
parameters <- c(
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
# MCMC  settings
niter <- 100000
nthin <- 10
nburn <- 30000
nchains <- 3
# Call  JAGS
out <- jags(bugs.data, inits, parameters, "model.jags", n.chains = nchains, n.thin = nthin, n.iter = niter, n.burnin = nburn, working.directory = getwd())
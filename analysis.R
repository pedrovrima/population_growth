

###  derive  data  for  the  model
# e = index  of the  earliest  observation
get.first <- function(x) min(which(x != 0))
e <- apply(CH, 1, get.first)
# l = index  of the  last  observation
get.last <- function(x) max(which(x != 0))
l <- apply(CH, 1, get.last)
# u = number  of  animals  observed  for the  first time at i
u <- matrix(0,2,dim(CH)[2])
u1 <- as.data.frame(table(e[which(sex==1)]))
u2 <- as.data.frame(table(e[which(sex==2)]))

u[1,as.numeric(levels(u1$Var1))] <- u1$Freq
u[2,as.numeric(levels(u2$Var1))] <- u2$Freq
# n = number  of  animals  observed  at i
n <- matrix(0,2,dim(CH)[2])
n[1,] <- colSums(CH[which(sex==1),])
n[2,] <- colSums(CH[which(sex==2),])
# v = number  of  animals  observed  for the  last  time at i
v <- matrix(0,2,dim(CH)[2])
v1 <- as.data.frame(table(l[which(sex==1)]))
v[1,as.numeric(levels(v1$Var1))] <- v1$Freq
v2 <- as.data.frame(table(l[which(sex==2)]))
v[2,as.numeric(levels(v2$Var1))] <- v2$Freq

# d = number  of  animals  removed  from  the  population  at time i
d <- matrix(0,2, dim(CH)[2])
# Time  covariate
time <- c(1:(dim(CH)[2] - 1))
# standardize  time
stand_time <- (time - mean(time)) / sd(time)
# Bundle  data
bugs.data <- list(u = u, n = n, v = v, d = d, s = dim(CH)[2], TIME = var)
##  Initial  values
inits <- function() {
    list(
        alpha.phi = runif(2, -2, 2),
        beta.time.phi = runif(1, -2, 2),
                beta.time.f = runif(1, -2, 2),

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
    "beta.time.f",
    
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
library(rjags)
out <- jags(bugs.data, inits, parameters, "model.jags", n.chains = nchains, n.thin = nthin, n.iter = niter, n.burnin = nburn, working.directory = getwd())
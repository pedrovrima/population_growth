#	(1)
##########################################
#	Simulate dataset: all constant
##########################################

# Starting population: 100 individuals
# constant phi (0.85)
# constant gamma (0.85)
# constant p (0.5)
# constant rho

n.occasions <- 10                         # Number of capture occasions
N1 <- 1000
phi <- rep(0.85, n.occasions-1)           # Survival probabilities
p <- rep(0.5, n.occasions)               # Capture probabilities


# empty survived CHs
CH.sur <- matrix(0, ncol = n.occasions, nrow = N1)
# All first 100 individuals are in the population
CH.sur[,1] <- 1   
# empty matrix for dead individuals
deaths <- matrix(NA, nrow=0, ncol=n.occasions)


#----------- Function to simulate capture-recapture data under Pradel (1996) model
simul.pradel <- function(phi, p, n.occasions, N1){

# Simulating survival (state) process
for (t in 1:(n.occasions-1)){
	for (i in 1:(dim(CH.sur)[1])){
		# Bernoulli trial: has individual survived occasion?
		sur <- rbinom(1, 1, phi[t])
		# if yes, put 1 at t+1
		if (sur==1) {CH.sur[i,(t+1)] <- 1}
	}#i
	# copy apart deaths CH
	deaths <- rbind(deaths, CH.sur[which(CH.sur[,(t+1)]==0),])
	# replace CHs of individuals dead at time t, with CHs of new recruited (between t an t+1) available at time t+1
	# i.e. we added the number of new individuals each time period needed to compensate for deaths 
	tmp <- matrix(0, ncol = n.occasions, nrow = length(which(CH.sur[,(t+1)]==0)))
	tmp[,(t+1)]<-1
	CH.sur[which(CH.sur[,(t+1)]==0),] <- tmp
} #t

# bind all CHs of all individuals ever entered the population
CH.tot <- rbind(CH.sur,deaths)

# check, actual population size
#colSums(CH.tot)
# [1] 100 100 100 100 100 100 100 100 100 100

# empty captured CHs
CH.p <- matrix(0, ncol = n.occasions, nrow = dim(CH.tot)[1])

# Simulating capture
for (i in 1:(dim(CH.tot)[1])){
	for (t in 1:n.occasions){
		# if individual is in the population it might be captured...
		if (CH.tot[i,t]==1) {CH.p[i,t] <- rbinom(1,1,p[t])}
	}#t
} #i

# Remove individuals never captured
capt.sum <- rowSums(CH.p)
never.capt <- which(capt.sum == 0)
CH <- CH.p[-never.capt,]
return(list(CH.obs=CH, CH.state=CH.tot))

}# end simulation function


# Execute simulation function
sim <- simul.pradel(phi, p, n.occasions, N1)

# observed CH
CH <- sim$CH.obs




# save 
save(CH, file="/media/data/PhD_11_SHEARW_Tenan_et_al/simulations/CH_simul_data_const.Rdata", ascii=TRUE)




#	(2)
##########################################
#	Simulate dataset: TREND in detection (0.3)
##########################################

# Starting population: 100 individuals
# constant phi (0.85)
# constant gamma (0.85)
# trend in p: mean p = 0.5; beta = 0.3
# constant rho

n.occasions <- 10                         		# Number of capture occasions
N1 <- 100
phi <- rep(0.85, n.occasions-1)           		# Survival probabilities
time <- c(1:10)
stand_time <- (time - mean(time)) / sd(time)	# standardize time
mean.p <- 0.5
beta <- 0.3										# temporal trend in detectability
logit.p <- qlogis(0.5) + beta * stand_time
p <- plogis(logit.p)


# empty survived CHs
CH.sur <- matrix(0, ncol = n.occasions, nrow = N1)
# All first 100 individuals are in the population
CH.sur[,1] <- 1   
# empty matrix for dead individuals
deaths <- matrix(NA, nrow=0, ncol=n.occasions)


#----------- Function to simulate capture-recapture data under Pradel (1996) model
simul.pradel <- function(phi, p, n.occasions, N1){

# Simulating survival (state) process
for (t in 1:(n.occasions-1)){
	for (i in 1:(dim(CH.sur)[1])){
		# Bernoulli trial: has individual survived occasion?
		sur <- rbinom(1, 1, phi[t])
		# if yes, put 1 at t+1
		if (sur==1) {CH.sur[i,(t+1)] <- 1}
	}#i
	# copy apart deaths CH
	deaths <- rbind(deaths, CH.sur[which(CH.sur[,(t+1)]==0),])
	# replace CHs of individuals dead at time t, with CHs of new recruited (between t an t+1) available at time t+1
	# i.e. we added the number of new individuals each time period needed to compensate for deaths 
	tmp <- matrix(0, ncol = n.occasions, nrow = length(which(CH.sur[,(t+1)]==0)))
	tmp[,(t+1)]<-1
	CH.sur[which(CH.sur[,(t+1)]==0),] <- tmp
} #t

# bind all CHs of all individuals ever entered the population
CH.tot <- rbind(CH.sur,deaths)

# check, actual population size
#colSums(CH.tot)
# [1] 100 100 100 100 100 100 100 100 100 100

# empty captured CHs
CH.p <- matrix(0, ncol = n.occasions, nrow = dim(CH.tot)[1])

# Simulating capture
for (i in 1:(dim(CH.tot)[1])){
	for (t in 1:n.occasions){
		# if individual is in the population it might be captured...
		if (CH.tot[i,t]==1) {CH.p[i,t] <- rbinom(1,1,p[t])}
	}#t
} #i

# Remove individuals never captured
capt.sum <- rowSums(CH.p)
never.capt <- which(capt.sum == 0)
CH <- CH.p[-never.capt,]
return(list(CH.obs=CH, CH.state=CH.tot))

}# end simulation function


# Execute simulation function
sim <- simul.pradel(phi, p, n.occasions, N1)

# observed CH
CH <- sim$CH.obs




# save 
save(CH, file="/media/data/PhD_11_SHEARW_Tenan_et_al/simulations/CH_simul_data_pTREND03.Rdata", ascii=TRUE)






#	(3)
##########################################
#	Simulate dataset: TREND in detection (0.6)
##########################################

# Starting population: 100 individuals
# constant phi (0.85)
# constant gamma (0.85)
# trend in p: mean p = 0.5; beta = 0.6
# constant rho

n.occasions <- 10                         		# Number of capture occasions
N1 <- 100
phi <- rep(0.85, n.occasions-1)           		# Survival probabilities
time <- c(1:10)
stand_time <- (time - mean(time)) / sd(time)	# standardize time
mean.p <- 0.5
beta <- 0.6										# temporal trend in detectability
logit.p <- qlogis(0.5) + beta * stand_time
p <- plogis(logit.p)


# empty survived CHs
CH.sur <- matrix(0, ncol = n.occasions, nrow = N1)
# All first 100 individuals are in the population
CH.sur[,1] <- 1   
# empty matrix for dead individuals
deaths <- matrix(NA, nrow=0, ncol=n.occasions)


#----------- Function to simulate capture-recapture data under Pradel (1996) model
simul.pradel <- function(phi, p, n.occasions, N1){

# Simulating survival (state) process
for (t in 1:(n.occasions-1)){
	for (i in 1:(dim(CH.sur)[1])){
		# Bernoulli trial: has individual survived occasion?
		sur <- rbinom(1, 1, phi[t])
		# if yes, put 1 at t+1
		if (sur==1) {CH.sur[i,(t+1)] <- 1}
	}#i
	# copy apart deaths CH
	deaths <- rbind(deaths, CH.sur[which(CH.sur[,(t+1)]==0),])
	# replace CHs of individuals dead at time t, with CHs of new recruited (between t an t+1) available at time t+1
	# i.e. we added the number of new individuals each time period needed to compensate for deaths 
	tmp <- matrix(0, ncol = n.occasions, nrow = length(which(CH.sur[,(t+1)]==0)))
	tmp[,(t+1)]<-1
	CH.sur[which(CH.sur[,(t+1)]==0),] <- tmp
} #t

# bind all CHs of all individuals ever entered the population
CH.tot <- rbind(CH.sur,deaths)

# check, actual population size
#colSums(CH.tot)
# [1] 100 100 100 100 100 100 100 100 100 100

# empty captured CHs
CH.p <- matrix(0, ncol = n.occasions, nrow = dim(CH.tot)[1])

# Simulating capture
for (i in 1:(dim(CH.tot)[1])){
	for (t in 1:n.occasions){
		# if individual is in the population it might be captured...
		if (CH.tot[i,t]==1) {CH.p[i,t] <- rbinom(1,1,p[t])}
	}#t
} #i

# Remove individuals never captured
capt.sum <- rowSums(CH.p)
never.capt <- which(capt.sum == 0)
CH <- CH.p[-never.capt,]
return(list(CH.obs=CH, CH.state=CH.tot))

}# end simulation function


# Execute simulation function
sim <- simul.pradel(phi, p, n.occasions, N1)

# observed CH
CH <- sim$CH.obs




# save 
save(CH, file="/media/data/PhD_11_SHEARW_Tenan_et_al/simulations/CH_simul_data_pTREND06.Rdata", ascii=TRUE)



#	(4)
##########################################
#	Simulate dataset: TREND in survival (0.3)
##########################################

# Starting population: 100 individuals
# trend in phi: mean phi = 0.7; beta = 0.3
# trend in gamma (= trend in phi)
# constant p (0.5)
# constant rho

n.occasions <- 10                         		# Number of capture occasions
N1 <- 100
time <- c(1:(n.occasions-1))
stand_time <- (time - mean(time)) / sd(time)	# standardize time
mean.phi <- 0.7
beta <- 0.3										# temporal trend in survival
logit.phi <- qlogis(mean.phi) + beta * stand_time
phi <- plogis(logit.phi)						# survival probability
p <- rep(0.5, n.occasions)			            # Capture probabilities




# empty survived CHs
CH.sur <- matrix(0, ncol = n.occasions, nrow = N1)
# All first 100 individuals are in the population
CH.sur[,1] <- 1   
# empty matrix for dead individuals
deaths <- matrix(NA, nrow=0, ncol=n.occasions)


#----------- Function to simulate capture-recapture data under Pradel (1996) model
simul.pradel <- function(phi, p, n.occasions, N1){

# Simulating survival (state) process
for (t in 1:(n.occasions-1)){
	for (i in 1:(dim(CH.sur)[1])){
		# Bernoulli trial: has individual survived occasion?
		sur <- rbinom(1, 1, phi[t])
		# if yes, put 1 at t+1
		if (sur==1) {CH.sur[i,(t+1)] <- 1}
	}#i
	# copy apart deaths CH
	deaths <- rbind(deaths, CH.sur[which(CH.sur[,(t+1)]==0),])
	# replace CHs of individuals dead at time t, with CHs of new recruited (between t an t+1) available at time t+1
	# i.e. we added the number of new individuals each time period needed to compensate for deaths 
	tmp <- matrix(0, ncol = n.occasions, nrow = length(which(CH.sur[,(t+1)]==0)))
	tmp[,(t+1)]<-1
	CH.sur[which(CH.sur[,(t+1)]==0),] <- tmp
} #t

# bind all CHs of all individuals ever entered the population
CH.tot <- rbind(CH.sur,deaths)

# check, actual population size
#colSums(CH.tot)
# [1] 100 100 100 100 100 100 100 100 100 100

# empty captured CHs
CH.p <- matrix(0, ncol = n.occasions, nrow = dim(CH.tot)[1])

# Simulating capture
for (i in 1:(dim(CH.tot)[1])){
	for (t in 1:n.occasions){
		# if individual is in the population it might be captured...
		if (CH.tot[i,t]==1) {CH.p[i,t] <- rbinom(1,1,p[t])}
	}#t
} #i

# Remove individuals never captured
capt.sum <- rowSums(CH.p)
never.capt <- which(capt.sum == 0)
CH <- CH.p[-never.capt,]
return(list(CH.obs=CH, CH.state=CH.tot))

}# end simulation function


# Execute simulation function
sim <- simul.pradel(phi, p, n.occasions, N1)

# observed CH
CH <- sim$CH.obs




# save 
save(CH, file="/media/data/PhD_11_SHEARW_Tenan_et_al/simulations/CH_simul_data_phiTREND03.Rdata", ascii=TRUE)




#	(5)
##########################################
#	Simulate dataset: TREND in survival (0.6)
##########################################

# Starting population: 100 individuals
# trend in phi: mean phi = 0.7; beta = 0.6
# trend in gamma (= trend in phi)
# constant p (0.5)
# constant rho

n.occasions <- 10                         		# Number of capture occasions
N1 <- 100
time <- c(1:(n.occasions-1))
stand_time <- (time - mean(time)) / sd(time)	# standardize time
mean.phi <- 0.7
beta <- 0.6										# temporal trend in survival
logit.phi <- qlogis(mean.phi) + beta * stand_time
phi <- plogis(logit.phi)						# survival probability
p <- rep(0.5, n.occasions)			            # Capture probabilities

#phi
#[1] 0.4927360 0.5473652 0.6008768 0.6520832 0.7000000 0.7439092 0.7833805
#[8] 0.8182543 0.8485989


# empty survived CHs
CH.sur <- matrix(0, ncol = n.occasions, nrow = N1)
# All first 100 individuals are in the population
CH.sur[,1] <- 1   
# empty matrix for dead individuals
deaths <- matrix(NA, nrow=0, ncol=n.occasions)


#----------- Function to simulate capture-recapture data under Pradel (1996) model
simul.pradel <- function(phi, p, n.occasions, N1){

# Simulating survival (state) process
for (t in 1:(n.occasions-1)){
	for (i in 1:(dim(CH.sur)[1])){
		# Bernoulli trial: has individual survived occasion?
		sur <- rbinom(1, 1, phi[t])
		# if yes, put 1 at t+1
		if (sur==1) {CH.sur[i,(t+1)] <- 1}
	}#i
	# copy apart deaths CH
	deaths <- rbind(deaths, CH.sur[which(CH.sur[,(t+1)]==0),])
	# replace CHs of individuals dead at time t, with CHs of new recruited (between t an t+1) available at time t+1
	# i.e. we added the number of new individuals each time period needed to compensate for deaths 
	tmp <- matrix(0, ncol = n.occasions, nrow = length(which(CH.sur[,(t+1)]==0)))
	tmp[,(t+1)]<-1
	CH.sur[which(CH.sur[,(t+1)]==0),] <- tmp
} #t

# bind all CHs of all individuals ever entered the population
CH.tot <- rbind(CH.sur,deaths)

# check, actual population size
#colSums(CH.tot)
# [1] 100 100 100 100 100 100 100 100 100 100

# empty captured CHs
CH.p <- matrix(0, ncol = n.occasions, nrow = dim(CH.tot)[1])

# Simulating capture
for (i in 1:(dim(CH.tot)[1])){
	for (t in 1:n.occasions){
		# if individual is in the population it might be captured...
		if (CH.tot[i,t]==1) {CH.p[i,t] <- rbinom(1,1,p[t])}
	}#t
} #i

# Remove individuals never captured
capt.sum <- rowSums(CH.p)
never.capt <- which(capt.sum == 0)
CH <- CH.p[-never.capt,]
return(list(CH.obs=CH, CH.state=CH.tot))

}# end simulation function


# Execute simulation function
sim <- simul.pradel(phi, p, n.occasions, N1)

# observed CH
CH <- sim$CH.obs




# save 
save(CH, file="/media/data/PhD_11_SHEARW_Tenan_et_al/simulations/CH_simul_data_phiTREND06.Rdata", ascii=TRUE)



























#-------------------------------------------------------------------------------


# 10.4. Models with constant survival and time-dependent entry
# Define parameter values
n.occasions <- 7                         # Number of capture occasions
N <- 400                                 # Superpopulation size
phi <- rep(0.7, n.occasions-1)           # Survival probabilities
b <- c(0.34, rep(0.11, n.occasions-1))   # Entry probabilities 
p <- rep(0.5, n.occasions)               # Capture probabilities

PHI <- matrix(rep(phi, (n.occasions-1)*N), ncol = n.occasions-1, nrow = N, byrow = T)
P <- matrix(rep(p, n.occasions*N), ncol = n.occasions, nrow = N, byrow = T)

# Function to simulate capture-recapture data under the JS model
simul.js <- function(PHI, P, b, N){
   B <- rmultinom(1, N, b) # Generate no. of entering ind. per occasion
   n.occasions <- dim(PHI)[2] + 1
   CH.sur <- CH.p <- matrix(0, ncol = n.occasions, nrow = N)
   # Define a vector with the occasion of entering the population
   ent.occ <- numeric()
   for (t in 1:n.occasions){
      ent.occ <- c(ent.occ, rep(t, B[t]))
      }
   # Simulating survival
   for (i in 1:N){
      CH.sur[i, ent.occ[i]] <- 1   # Write 1 when ind. enters the pop.
      if (ent.occ[i] == n.occasions) next
      for (t in (ent.occ[i]+1):n.occasions){
         # Bernoulli trial: has individual survived occasion?
         sur <- rbinom(1, 1, PHI[i,t-1])
         ifelse (sur==1, CH.sur[i,t] <- 1, break)
         } #t
      } #i
   # Simulating capture
   for (i in 1:N){
      CH.p[i,] <- rbinom(n.occasions, 1, P[i,])
      } #i
   # Full capture-recapture matrix
   CH <- CH.sur * CH.p
   
   # Remove individuals never captured
   cap.sum <- rowSums(CH)
   never <- which(cap.sum == 0)
   CH <- CH[-never,]
   Nt <- colSums(CH.sur)    # Actual population size
   return(list(CH=CH, B=B, N=Nt))
   }

# Execute simulation function
sim <- simul.js(PHI, P, b, N)
CH <- sim$CH

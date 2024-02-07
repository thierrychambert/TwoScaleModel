## Classical demographic model
rm(list = ls(all.names = TRUE))

# loading the required packages
library(jagsUI)
library(magrittr)

## Save the data file
data_count <- read.csv(file = "shag_count_data.csv")
PI <- read.csv(file = "shag_prop_surveyed.csv")

# Define max.N0 for the model
max.N0 <- round(sum(data_count[,1]/PI[,1], na.rm = TRUE)*1.5)

# Add NA's to automatically project the demography into the future
ny0 <- ncol(data_count)
data_count[, (ny0+1:30)] <- NA
colnames(data_count)[ny0+(1:30)] <- paste0("X", as.numeric(gsub("X", "", names(data_count)[ny0])) + (1:30))
PI[, (ny0+1:30)] <- 1

# Data bundle
jags.data <- list(y=data_count, I = nrow(data_count), T=ncol(data_count), 
                  max.N0 = max.N0, PI=PI)

# Write JAGS model file ####
cat(file = "classical_model.txt", "
  model{
    
    # Prior on lambda (population growth rate)
    mu.lam_0 ~ dunif(0, 10)
    mu.lam_0_log <- log(mu.lam_0)
  
    sig.lam_i ~ dunif(0, 2)
    sig.lam_i_log <- sig.lam_i/mu.lam_0
    tau.lam_i_log <- pow(sig.lam_i_log, -2)

    # Likelihood
    # Model for N0 : uniform priors
    for(i in 1:I){
      N[i,1] ~ dunif(0, max.N0)
    } # i
    
    # Process model over time
    for(i in 1:I){
    lambda_log[i] ~ dnorm(mu.lam_0_log, tau.lam_i_log)
    lambda[i] <- exp(lambda_log[i])
      for (t in 1:(T-1)){
        N[i,t+1] <- N[i,t] * lambda[i]
      } # t
    } # i
    
    ## Observation process
    for(i in 1:I){
      for(t in 1:T){
        y[i,t] ~ dpois(N[i,t]*PI[i,t])
        n[i,t] <- N[i,t]
      } # t
    } # i
    
  } # model
") # cat

# Initial values
inits <- function(){
  list(
    mu.lam_0 = runif(1, 1, 2)
  )
}
inits()

# Parameters monitored
parameters <- c("mu.lam_0", "sig.lam_i", "n")

# MCMC settings
nc <- 3
ni <- 20000
nb <- 1000
na <- 5000
nt <- 5

# Call JAGS
system.time(
  out <- jags(data = jags.data, inits = NULL, 
              parameters.to.save = parameters, 
              model.file = "classical_model.txt", 
              n.chains = nc, n.iter = ni, 
              n.burnin = nb, n.adapt = na,
              n.thin = nt, DIC = FALSE, 
              parallel = FALSE
  ) # close jags fn
) # clos time fn


# Look at output summary
out$summary[, c(1,3,7,8:9)] %>% round(.,3) %>% head(., 30)

## Plot results
x11()
source("03_Plot_res.R")



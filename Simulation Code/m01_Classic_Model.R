# Write JAGS model file ####
m01 <- nimbleCode( { 
  
  # Prior on lambda (population growth rate)
  mu.lam_0 ~ dunif(0, 10)
  mu.lam_0_log <- log(mu.lam_0)
  
  sig.lam_i ~ dunif(0, 2)
  sig.lam_i_log <- sig.lam_i/mu.lam_0
  tau.lam_i_log <- pow(sig.lam_i_log, -2)
  
  # Likelihood
  # Model for N0 : uniform priors
  for(i in 1:I){
    n[i,1] ~ dunif(0, max.N0)
  } # i
  
  # Process model over time
  for(i in 1:I){
    lambda_log[i] ~ dnorm(mu.lam_0_log, tau.lam_i_log)
    lambda[i] <- exp(lambda_log[i])
    for (t in 1:(T-1)){
      n[i,t+1] <- n[i,t] * lambda[i]
    } # t
  } # i
  
  ## Observation process
  for(i in 1:I){
    for(t in 1:T){
      y[i,t] ~ dnorm(n[i,t], 1000)
    } # t
  } # i
  
  
  ## Derived parameters 
  # growth_i (colony scale)
  for(i in 1:I){
    growth_i[i] <- pow((n[i,T]/n[i,1]), (1/(T-1)))
  } # i
  
  # Global growth rate (all colonies = global scale)
  for(t in 1:T){
    N[t] <- sum(n[1:I,t])
  } # t
  lambda_G <- pow((N[T]/N[1]), (1/(T-1)))
  
} ) # End model description
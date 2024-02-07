# Classic Model #####
m02 <- nimbleCode( { 
  
  ## National Scale
  # Prior on lambda_G (population growth rate at global scale)
  lambda_G ~ dunif(0, 10)
  
  # Prior on N0
  N[1] ~ dunif(0, max.N0)
  
  # Process model over time (population growth)
  for (t in 1:(T-1)){
    N[t+1] <- N[t] * lambda_G
  } # t
  
  ## Colony-groups Scale
  # Priors
  for(i in 1:I){
    b0[i] ~ dunif(0,100)
    b1[i] ~ dunif(0,100)
  } # i
  
  for(t in 1:T){
    for(i in 1:I){
      w[i,t] <- b0[i] + b1[i]*(t-1)
      gamma[i,t] <- w[i,t]/sum(w[1:I,t])
    } # i
  } # t
  
  # Process model : allocation of N among colony-groups 
  for(t in 1:T){
    n[1:I,t] <- gamma[1:I,t]*N[t]
  } # t
  
  ## Observation process
  for(i in 1:I){
    for(t in 1:T){
      y[i,t] ~ dnorm(n[i,t], 1000)
    } # t
  } # i
  
  ## Derived parameters : growth_i (colony scale)
  for(i in 1:I){
    growth_i[i] <- pow((n[i,T]/n[i,1]), (1/(T-1)))
  } # i
  
} ) # End model description

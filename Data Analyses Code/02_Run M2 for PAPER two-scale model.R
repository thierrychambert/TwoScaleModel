## Two-scale demographic model
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

# Fit the data model #####
# Data bundle
jags.data <- list(y=data_count, I = nrow(data_count), T=ncol(data_count), 
                  max.N0 = max.N0, PI=PI)



# Write JAGS model file ####
cat(file = "twoscale_model.txt", "
  model{
    
    ## National Scale
    # Prior on lambda (population growth rate)
    mu.lam_0 ~ dunif(0, 10)

    # Prior on N0
    N[1] ~ dunif(0, max.N0)

    # Process model over time (population growth)
    for (t in 1:(T-1)){
      lambda[t] <- mu.lam_0
      N[t+1] <- N[t] * lambda[t]
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
    
    # Process model : repartition of N among colony-groups 
    for(t in 1:T){
      NN[t] <- round(N[t])
      nb[1:I,t] <- gamma[1:I,t]*NN[t]
      n[1:I,t] <- nb[1:I,t]
    } # t
    
    ## Observation process
    for(i in 1:I){
      for(t in 1:T){
        y[i,t] ~ dpois(n[i,t]*PI[i,t])
      } # t
    } # i
    
  } # model
") # cat

# Initial values
inits <- function(){
  list(
    N = round(sum(data_count[,1], na.rm = TRUE)*(1+runif(1,0,0.5))),
    n = round(data_count/PI)
  )
}
inits()

# Parameters monitored
parameters <- c("mu.lam_0", "b0", "b1", "N", "n", "gamma")

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
              model.file = "twoscale_model.txt", 
              n.chains = nc, n.iter = ni, 
              n.burnin = nb, n.adapt = na,
              n.thin = nt, DIC = FALSE, 
              parallel = FALSE
  ) # close jags fn
) # clos time fn



# Look at output summary
out$summary[, c(1,3,7,8:9)] %>% round(.,3) %>% head(., 30)

# load results (already run)
#load("./output_csv/res_M2.rda")

## Plot results
x11()
source("03_Plot_res.R")


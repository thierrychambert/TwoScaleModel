options(digits = 7, scipen = 999)
rm(list = ls(all.names = TRUE))

## Source codes
source("s01_load_packages.R")
source("f01_move_mat_fn.R")
source("f02_simul_fn.R")
source("f03_extract_estimates_fn.R")
source("m01_Classic_Model.R")
source("m02_TwoScale_Model.R")
source("f05_perf_metrics_fn.R")
source("p02_plot_res_Nimble.R")
nimbleOptions(verbose = FALSE)


# set.seed(834)
predictif = TRUE

## Parameters #####
nb_colo = 8
nb_years = 30
ny_suivi = 10
N0_tot = nb_colo*30

## Pop trend
lambda_i <- c(0.96, 0.98, 1.01, 1.03, 1.04, 1.06, 1.07, 1.10)
lambda_i <- sample(lambda_i) # break order
# lambda_i <- lambda_i - 0.05
mean(lambda_i)

site_fidelity = 0.9
heterog_attrac_colo = 0.75

propNA <- 0

min_lam = NA
max_lam = NA


## MCMC specs
niter = 11000
nburnin = 1000
nthin = 1
nchains = 3


## Object to save perf metrics
rel_rmse <- list()
cl_rel_rmse <- list() 

## Simulate data #####
simul_out <- simul_traj(nb_colo = nb_colo, nb_years = nb_years, 
                        N0_tot = N0_tot, 
                        min_lam = min_lam, max_lam = max_lam,
                        lambda_i = lambda_i,
                        heterog_attrac_colo = heterog_attrac_colo, 
                        site_fidelity = site_fidelity,
                        propNA = propNA)

## Extract objects
n <- simul_out$real_traj
y <- simul_out$obs_data
growth_i <- simul_out$growth_i
lambda_global <- simul_out$lambda_global


## make NA after year 10
if(predictif) y[,(ny_suivi+1):nb_years] <- NA



# Define max.N0 for the model
# max.N0 <- round(sum(y[,1], na.rm = TRUE)*5)
max.N0_m01 <- (y %>% max(., na.rm = TRUE) * 2) %>% round(., -1) ; max.N0
max.N0_m02 <- round(sum(y[,1], na.rm = TRUE)*5)

## Define constants
Consts_m01 <- list(I=nrow(y), T=ncol(y), max.N0 = max.N0_m01)
Consts_m02 <- list(I=nrow(y), T=ncol(y), max.N0 = max.N0_m02)

## Compile models #####
# 1. Create the models
model1 <- nimbleModel(code = m01, name = "model1", constants = Consts_m01)
model2 <- nimbleModel(code = m02, name = "model2", constants = Consts_m02)

# 2. Set data & Inits
model1$setData(list(y=y))
model2$setData(list(y=y))

# 3. Configure the MCMC
model1Conf <- configureMCMC(model1, print = TRUE)
model2Conf <- configureMCMC(model2, print = TRUE)

# 4. Add monitors
# Parameters monitored
model1Conf$addMonitors(c("lambda_G", "growth_i", "n"))
model2Conf$addMonitors(c("lambda_G", "growth_i", "n"))

# 5. Build MCMC
model1MCMC <- buildMCMC(model1Conf)
model2MCMC <- buildMCMC(model2Conf)

## 6. Compile the model (= generate C++ code) & Compile the MCMC algo
# Model 1
Comp_model1 <- compileNimble(model1)
Comp_model1_MCMC <- compileNimble(model1MCMC, project = model1)
print("model 1 compiled")
write("model 1 compiled", "./outputs/avancement.txt", append = TRUE) 

# Model 2
Comp_model2 <- compileNimble(model2)
Comp_model2_MCMC <- compileNimble(model2MCMC, project = model2)
print("model 2 compiled")
write("model 2 compiled", "./outputs/avancement.txt", append = TRUE) 


### Run Analaysis #####
## Model 1
run_m1 <- runMCMC(Comp_model1_MCMC,
                  thin = nthin, 
                  niter = niter, 
                  nburnin = nburnin, 
                  nchains = nchains)

# Extract estimates
res_m1 <- extract_estimates(run_m1)


## Model 2
run_m2 <- runMCMC(Comp_model2_MCMC,
                  thin = nthin, 
                  niter = niter, 
                  nburnin = nburnin, 
                  nchains = nchains)

# Extract estimates
res_m2 <- extract_estimates(run_m2)



## Plots
x11()
plot_res_n(real_n = n, est_n = res_m1$est_n, lci_n = res_m1$lci_n, uci_n = res_m1$uci_n, 
           show_legend = FALSE, title = "Classic demog model")

x11()
plot_res_n(real_n = n, est_n = res_m2$est_n, lci_n = res_m2$lci_n, uci_n = res_m2$uci_n, 
           show_legend = FALSE, title = "Two-Scale model")
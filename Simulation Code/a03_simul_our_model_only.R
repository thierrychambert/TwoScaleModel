options(digits = 7, scipen = 999)
rm(list = ls(all.names = TRUE))
source("s01_load_packages.R")
source("f01_move_mat_fn.R")
source("f02_simul_fn.R")
source("f03_analysis_NIMBLE_fn.R")
source("f04_classic_model_fn.R")
source("f05_perf_metrics_fn.R")
source("p02_plot_res_Nimble.R")
nimbleOptions(verbose = FALSE)


## Parameters #####
nb_colo = 8
nb_years = 30
N0_tot = nb_colo*30

min_lam = NA
max_lam = NA
lambda_i <- c(0.95, 0.98, 1.02, 1.03, 1.04, 1.06, 1.07, 1.10)

heterog_attrac_colo = 1
site_fidelity = 0.7

propNA <- 0

## 1 scenario
nsim <- 100

## Object to save perf metrics
rel_rmse <- rel_bias <- list()
cl_rel_rmse <- cl_rel_bias <- list()
#err_nT_max <- cl_err_nT_max <- NA
rel_err <- cl_rel_err <- list()

sim=1

## Run Loops over SIMS
time_run <- system.time(
  for(sim in 1:nsim){
    
    print(paste("simul",sim))
    
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
    y[,10:nb_years] <- NA
    
    ### Run analysis #####
    ## From our new model
    run <- run_analysis(y = y)
    
    ### Outputs #####
    ## Calculate RELATIVE RMSE and ERROS
    sel <- which.max(run$est_n[,nb_years])
    
    # From the new model
    rel_rmse$n[sim] <- get_rel_rmse(est = run$est_n, real = n)
    rel_rmse$growth_i[sim] <- get_rel_rmse(est = run$est_growth_i, real = growth_i)
    rel_rmse$nT[sim] <- get_rel_rmse(est = run$est_n[,nb_years], real = n[,nb_years])
    
    rel_err$lamG[sim] <- (run$est_lamG - lambda_global)/lambda_global
    rel_err$nT_max[sim] <- (run$est_n[sel,nb_years] -  n[sel,nb_years])/n[sel,nb_years]
    
  } # sim
  
) # end system.time
time_run


## Ratio of errors
MEAN <- function(x) mean(x, na.rm = TRUE)

## Save results
tbl <- as.data.frame(
  rbind(
    rmse_growth.i = c(rel_rmse$growth_i %>% MEAN, rel_rmse$growth_i %>% MEAN),
    rmse_n = c(rel_rmse$n %>% MEAN, rel_rmse$n %>% MEAN),
    rmse_nT = c(rel_rmse$nT %>% MEAN, rel_rmse$nT %>% MEAN),
    
    ## Bias
    bias_lamG = c(rel_err$lamG %>% MEAN, rel_err$lamG %>% MEAN),
    bias_nT.max = c(rel_err$nT_max %>% MEAN, rel_err$nT_max %>% MEAN),
    
    ## ABS error mean
    abs.err_lamG = c(rel_err$lamG %>% abs %>% MEAN, rel_err$lamG %>% abs %>% MEAN),
    abs.err_nT.max = c(rel_err$nT_max %>% abs %>% MEAN, rel_err$nT_max %>% abs %>% MEAN),
    
    ## RMSE
    rmse_lamG = c( (rel_err$lamG %>% pow(.,2) %>% MEAN %>% sqrt),  (rel_err$lamG %>% pow(.,2) %>% MEAN %>% sqrt) ),
    rmse_nT.max = c((rel_err$nT_max %>% pow(.,2) %>% MEAN %>% sqrt) , (rel_err$nT_max %>% pow(.,2) %>% MEAN %>% sqrt) )
  )
)

names(tbl) <- c("Our model", "Our model")
tbl
tbl[,1]/tbl[,2]

write.table(tbl, file = "./outputs/perf_res_our_model_only.csv", sep = ";")

## Plots
#plot_res_n(real_n = n, est_n = run$est_n, lci_n = run$lci_n, uci_n = run$uci_n, title = "New model")
#plot_res_n(real_n = n, est_n = cl_run$est_n, lci_n = cl_run$lci_n, uci_n = cl_run$uci_n, title = "Classic demog model")

## Extract performance metrics
#rel_bias[sim] <- get_rel_bias(est = run$est_n, real = n)
#rel_rmse[sim] <- get_rel_rmse(est = run$est_n, real = n)
#rel_bias %>% MEAN
#rel_rmse %>% MEAN


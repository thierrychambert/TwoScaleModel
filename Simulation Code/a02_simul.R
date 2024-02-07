options(digits = 7, scipen = 999)
rm(list = ls(all.names = TRUE))

## Start clock
start_time <- Sys.time()
write(paste("Start time : ", start_time), "./outputs/avancement.txt", append = FALSE) 

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

MEAN <- function(x) mean(x, na.rm = TRUE)

## nb sims / scenario
nsim <- 1000

## Pop trend
lambda_i <- c(0.96, 0.98, 1.01, 1.03, 1.04, 1.06, 1.07, 1.10)
lambda_i <- sample(lambda_i) # break order
# lambda_i <- lambda_i - 0.05
mean(lambda_i)

## Scenarios
site_fidelity_list = 0.7# c(0.3, 0.5, 0.7, 0.9)
heterog_attrac_colo_list = 0.75# c(0.5, 0.75, 1)

AA <- length(site_fidelity_list)
BB <- length(heterog_attrac_colo_list)

nsc <- AA*BB
nsc

## Parameters #####
nb_colo = 8
nb_years = 30
ny_suivi = 10
N0_tot = nb_colo*30

min_lam = NA
max_lam = NA

propNA <- 0


## MCMC specs
niter = 11000
nburnin = 1000
nthin = 1
nchains = 3


### Initialize models #####
## Simulate data
simul_out <- simul_traj(nb_colo = nb_colo, nb_years = nb_years, 
                        N0_tot = N0_tot, 
                        min_lam = min_lam, max_lam = max_lam,
                        lambda_i = lambda_i,
                        heterog_attrac_colo = heterog_attrac_colo_list[1], 
                        site_fidelity = site_fidelity_list[1],
                        propNA = propNA)

## Extract objects
y <- simul_out$obs_data

## make NA after year 10
y[,(ny_suivi+1):nb_years] <- NA

# Define max.N0 for the model
# max.N0 <- round(sum(y[,1], na.rm = TRUE)*5)
max.N0_m01 <- (y %>% max(., na.rm = TRUE) * 2) %>% round(., -1)
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

## Temps de compilation
write(
  paste("temps de compilation :", 
        round( difftime(Sys.time(), start_time, units = "sec")[[1]] ), 
        "sec"), 
  file = "./outputs/avancement.txt", append = TRUE)
write("###############", "./outputs/avancement.txt", append = TRUE) 

## Objects
tbl_rel_rmse_n <- tbl_rel_rmse_growth_i <- tbl_rel_rmse_lambda_G <- data.frame(sc = 1:nsc)
tbl_rel_bias_n <- tbl_rel_bias_growth_i <- tbl_rel_bias_lambda_G <- data.frame(sc = 1:nsc)

tbl_rel_rmse_n[,"Metric"] <- "rel rmse on n"
tbl_rel_rmse_growth_i[,"Metric"] <- "rel rmse on growth_i"
tbl_rel_rmse_lambda_G[,"Metric"] <- "rel rmse on lambda_G"

tbl_rel_bias_n[,"Metric"] <- "rel bias on n"
tbl_rel_bias_growth_i[,"Metric"] <- "rel bias on growth_i"
tbl_rel_bias_lambda_G[,"Metric"] <- "rel bias on lambda_G"

tbl_rel_err_nT_max <- tbl_rel_rmse_nT <- tbl_rmse_rel_nT <- data.frame(sc = 1:nsc)
tbl_rel_err_nT_max[,"Metric"] <- "rel error on nT_max"
tbl_rel_rmse_nT[,"Metric"] <- "rel rmse on nT"
tbl_rmse_rel_nT[,"Metric"] <- "RMSE REL on nT"


sc <- 0
aa = bb = 1

## Start clock for loops
rm(start_time)
start_time <- Sys.time()
write(paste("Loops start now : ", start_time), "./outputs/avancement.txt", append = TRUE) 


## Loops over scenarios #####
time_run <- system.time(
  for(aa in 1:AA){
    for(bb in 1:BB){
      
      sc = sc + 1
      
      print("############################")
      print("############################")
      print("############################")
      print("############################")
      print(paste("scenario", sc))
      print("############################")
      
      ## Avancement scénario
      write(print("###############"), "./outputs/avancement.txt", append = TRUE) 
      write(paste("scenario", sc), "./outputs/avancement.txt", append = TRUE) 
      
      
      ## Parameters values for this scenario
      site_fidelity = site_fidelity_list[aa]
      heterog_attrac_colo = heterog_attrac_colo_list[bb]
      
      tbl_rel_rmse_n[sc, "Site_Fidelity"] <- site_fidelity
      tbl_rel_rmse_n[sc, "Heterog_Attrac"] <- heterog_attrac_colo
      
      ## Object to save perf metrics
      m1_rel_rmse_n <- m1_rel_rmse_growth_i <- m1_rel_rmse_lambda_G <- NA
      m2_rel_rmse_n <- m2_rel_rmse_growth_i <- m2_rel_rmse_lambda_G <- NA
      
      m1_rel_bias_n <- m1_rel_bias_growth_i <- m1_rel_bias_lambda_G <- NA
      m2_rel_bias_n <- m2_rel_bias_growth_i <- m2_rel_bias_lambda_G <- NA
      
      m1_rel_err_nT_max <- m1_rel_rmse_nT <- m1_rmse_rel_nT <- NA
      m2_rel_err_nT_max <- m2_rel_rmse_nT <- m2_rmse_rel_nT <- NA
      
      sim = 1
      ## Run Loops over SIMS
      time_scenario <- system.time(
        for(sim in 1:nsim){
          
          print("############################")
          print("############################")
          print(paste("simul", sim))
          print("############################")
          print("############################")
          
          ## Avancement simul
          if((sim %% (nsim/10)) == 0) write(paste("simul", sim), "./outputs/avancement.txt", append = TRUE) 
          
          ## Simulate data #####
          simul_out <- simul_traj(nb_colo = nb_colo, nb_years = nb_years, 
                                  N0_tot = N0_tot, 
                                  min_lam = min_lam, max_lam = max_lam,
                                  lambda_i = lambda_i,
                                  heterog_attrac_colo = heterog_attrac_colo, 
                                  site_fidelity = site_fidelity,
                                  propNA = propNA)
          
          ## Extract objects
          y <- simul_out$obs_data
          n <- simul_out$real_traj
          growth_i <- simul_out$growth_i
          lambda_G <- simul_out$lambda_global
          
          ## make NA after year 10
          y[,(ny_suivi+1):nb_years] <- NA
          
          # Define a new data set
          Comp_model1$y <- y
          Comp_model2$y <- y
          
          
          ## From model 1 
          # Re-compile the MCMC algo (step 6-b)
          Comp_model1_MCMC <- compileNimble(model1MCMC, project = model1)
          
          # Run MCMC (sampling) 
          run_m1 <- runMCMC(Comp_model1_MCMC,
                            thin = nthin, 
                            niter = niter, 
                            nburnin = nburnin, 
                            nchains = nchains)
          
          # Extract estimates
          res_m1 <- extract_estimates(run_m1)
          
          
          ## From model 2
          # Re-compile the MCMC algo (step 6-b)
          Comp_model2_MCMC <- compileNimble(model2MCMC, project = model2)
          
          # Run MCMC (sampling) 
          run_m2 <- runMCMC(Comp_model2_MCMC,
                            thin = nthin, 
                            niter = niter, 
                            nburnin = nburnin, 
                            nchains = nchains)
          
          # Extract estimates
          res_m2 <- extract_estimates(run_m2)
          
          
          ### Outputs #####
          
          
          
          ##-----------------------------------------------------##
          ## Error on fastest growing pop 
          sel <- which.max(res_m1$est_n[,nb_years])
          m1_rel_err_nT_max[sim] <- (res_m1$est_n[sel,nb_years] -  n[sel,nb_years])/n[sel,nb_years]
          m2_rel_err_nT_max[sim] <- (res_m2$est_n[sel,nb_years] -  n[sel,nb_years])/n[sel,nb_years]
          
          
          ##-----------------------------------------------------##
          m1_rel_rmse_nT[sim] <- get_rel_rmse(est = res_m1$est_n[,nb_years], real = n[,nb_years])
          m2_rel_rmse_nT[sim] <- get_rel_rmse(est = res_m2$est_n[,nb_years], real = n[,nb_years])
          
          ##-----------------------------------------------------##
          
          m1_rmse_rel_nT[sim] <- (get_rmse(est = res_m1$est_n[,nb_years], real = n[,nb_years]) / mean(n[,nb_years]))
          m2_rmse_rel_nT[sim] <- (get_rmse(est = res_m2$est_n[,nb_years], real = n[,nb_years]) / mean(n[,nb_years]))
          ##-----------------------------------------------------##
          
          
          

          ### RMSE
          m1_rel_rmse_n[sim] <- (get_rmse(est = res_m1$est_n, real = n) / mean(n))
          m2_rel_rmse_n[sim] <- (get_rmse(est = res_m2$est_n, real = n) / mean(n))
          
          m1_rel_rmse_growth_i[sim] <- (get_rmse(est = res_m1$est_growth_i, real = growth_i) / mean(growth_i))             # get_rel_rmse(est = run$est_growth_i, real = growth_i)
          m2_rel_rmse_growth_i[sim] <- (get_rmse(est = res_m2$est_growth_i, real = growth_i) / mean(growth_i))
          
          m1_rel_rmse_lambda_G[sim] <- get_rel_rmse(est = res_m1$est_lambda_G, real = lambda_G)
          m2_rel_rmse_lambda_G[sim] <- get_rel_rmse(est = res_m2$est_lambda_G, real = lambda_G)
          
          ### BIAS
          m1_rel_bias_n[sim] <- (get_bias(est = res_m1$est_n, real = n) / mean(n))
          m2_rel_bias_n[sim] <- (get_bias(est = res_m2$est_n, real = n) / mean(n))
          
          m1_rel_bias_growth_i[sim] <- (get_bias(est = res_m1$est_growth_i, real = growth_i) / mean(growth_i))
          m2_rel_bias_growth_i[sim] <- (get_bias(est = res_m2$est_growth_i, real = growth_i) / mean(growth_i))
          
          m1_rel_bias_lambda_G[sim] <- get_rel_bias(est = res_m1$est_lambda_G, real = lambda_G)
          m2_rel_bias_lambda_G[sim] <- get_rel_bias(est = res_m2$est_lambda_G, real = lambda_G)
          
        } # sim
      ) # end system.time

      
      ## Temps scénario
      write(print("######"), "./outputs/avancement.txt", append = TRUE) 
      write(paste("fin scenario", sc), "./outputs/avancement.txt", append = TRUE) 
      write(paste("Temps écoulé pour ce scénario :", 
                  round(time_scenario["elapsed"]), "sec"), 
            file = "./outputs/avancement.txt", append = TRUE)
      
      
      ## Save results
      
      
      ##-----------------------------------------------------##
      ## Error on fastest growing pop 
      tbl_rel_err_nT_max[sc, "Model1"] <- m1_rel_err_nT_max %>% MEAN
      tbl_rel_err_nT_max[sc, "Model2"] <- m2_rel_err_nT_max %>% MEAN
      
      ## RMSE Last Year
      tbl_rel_rmse_nT[sc, "Model1"] <- m1_rel_rmse_nT %>% MEAN
      tbl_rel_rmse_nT[sc, "Model2"] <- m2_rel_rmse_nT %>% MEAN

      
      tbl_rmse_rel_nT[sc, "Model1"] <- m1_rmse_rel_nT %>% MEAN
      tbl_rmse_rel_nT[sc, "Model2"] <- m2_rmse_rel_nT %>% MEAN
      ##-----------------------------------------------------##
      
      
      
      # RMSE
      tbl_rel_rmse_n[sc, "Model1"] <- m1_rel_rmse_n %>% MEAN
      tbl_rel_rmse_n[sc, "Model2"] <- m2_rel_rmse_n %>% MEAN
      
      tbl_rel_rmse_growth_i[sc, "Model1"] <- m1_rel_rmse_growth_i %>% MEAN
      tbl_rel_rmse_growth_i[sc, "Model2"] <- m2_rel_rmse_growth_i %>% MEAN
      
      tbl_rel_rmse_lambda_G[sc, "Model1"] <- m1_rel_rmse_lambda_G %>% MEAN
      tbl_rel_rmse_lambda_G[sc, "Model2"] <- m2_rel_rmse_lambda_G %>% MEAN
      
      # BIAS
      tbl_rel_bias_n[sc, "Model1"] <- m1_rel_bias_n %>% MEAN
      tbl_rel_bias_n[sc, "Model2"] <- m2_rel_bias_n %>% MEAN
      
      tbl_rel_bias_growth_i[sc, "Model1"] <- m1_rel_bias_growth_i %>% MEAN
      tbl_rel_bias_growth_i[sc, "Model2"] <- m2_rel_bias_growth_i %>% MEAN
      
      tbl_rel_bias_lambda_G[sc, "Model1"] <- m1_rel_bias_lambda_G %>% MEAN
      tbl_rel_bias_lambda_G[sc, "Model2"] <- m2_rel_bias_lambda_G %>% MEAN
      
      ## Temps depuis début
      print(round((Sys.time() - start_time), 2))
      duration <- difftime(Sys.time(), start_time)
      
      units(duration)
      
      write(
        paste("Temps écoulé depuis le début :", 
              round(duration[[1]], 2),
              units(duration)), 
        file = "./outputs/avancement.txt", append = TRUE)
      write(print("###############"), "./outputs/avancement.txt", append = TRUE) 
      
      
    } # bb
  } ## aa
) # end system.time
print(time_run)

## Temps total des boucles (scenarios x simulations)
write(print("###############"), "./outputs/avancement.txt", append = TRUE) 
write(
  paste("temps total des boucles (scenarios x simulations) :", 
        round( difftime(Sys.time(), start_time, units = "sec")[[1]] ), 
        "sec"), 
  file = "./outputs/avancement.txt", append = TRUE)

### Combine all tables
tbl <- rbind(tbl_rel_rmse_n[-which(names(tbl_rel_rmse_n) %in% c("Site_Fidelity","Heterog_Attrac"))], 
             tbl_rel_rmse_growth_i,
             tbl_rel_rmse_lambda_G,
             tbl_rel_bias_n,
             tbl_rel_bias_growth_i,
             tbl_rel_bias_lambda_G,
             
             tbl_rel_err_nT_max,
             tbl_rel_rmse_nT,
             tbl_rmse_rel_nT
)

## Add info
tbl[, c("Site_Fidelity","Heterog_Attrac")] <- tbl_rel_rmse_n[,c("Site_Fidelity","Heterog_Attrac")]

## Reorder columns
tbl <- tbl %>% 
  relocate(Model1, .after = last_col()) %>% 
  relocate(Model2, .after = last_col()) %>% 
  relocate(Metric, .before = Model1)

## Add nb sims
tbl$nsim <- nsim
tbl

## Save
write.table(tbl, file = "./outputs/perf_m1VSm2_TEST.csv", sep = ";", row.names = FALSE)


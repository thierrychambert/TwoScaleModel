## Specs, Hypothèses
# Observation counts : Poisson error
simul_traj <- function(nb_colo = 10, nb_years = 30, N0_tot = 500,
                       lambda_i = NULL,
                       min_lam = 0.98, max_lam = 1.08,
                       heterog_attrac_colo = 1, site_fidelity = 0.5,
                       propNA = 0.5){
  
  ## Parameters
  I <- nb_colo  # nb of colonies/sub-populations
  T <- nb_years  # nb of years
  
  ## Initial Meta-pop size (total)
  N0 <- N0_tot
  
  ## Pop size of each group
  n <- matrix(NA, nrow = I, ncol = T)
  n[,1] <- rmultinom(1, size = N0, prob = runif(I)) %>% sapply(., FUN = function(x) max(x, 1))
  
  ## Lambda of each group
  if(is.null(lambda_i)[1] | is.na(lambda_i)[1]) lambda_i <- runif(I, min_lam, max_lam)

  ## Movement among sites/groups
  make_warnings(heterog_attrac_colo, site_fidelity)
  heterog_attrac_colo <- min(max(heterog_attrac_colo, 0), 1)
  site_fidelity <- min(max(site_fidelity, 0), 1)
  
  ## Build the movement matrix
  move_mat <- build_move_mat(I = I, d = heterog_attrac_colo, site_fidelity = site_fidelity)
  
  # Process model over time (population growth)
  for (t in 1:(T-1)){
    
    # 1. Internal dynamic (survival & reproduction)
    n_tmp <- round(n[,t] * lambda_i)
    #n_tmp <- rpois(I, n[,t] * lambda_i)
    
    # 2. Movement among groups
    n[,t+1] <- round(n_tmp %*% move_mat)
    
  } # t
  
  ## Observation process
  y <- n
  
  # Random allocation of NA's
  whichNA <- sample.int(n = length(n), size = round(propNA*length(n)))
  y[whichNA] <- NA 
  
  ## Derived parameters
  # Growth rate (lambda + mvts) of each colony
  growth_i <- c()
  nyrs <- apply(n, 1, function(x) max(which(x > 0)))
  for(i in 1:nb_colo){
    growth_i[i] <- (n[i,nyrs[i]]/n[i,1])^(1/(nyrs[i]-1))
  } # i

  
  # Global growth rate (all colonies = global scale)
  N <- colSums(n)
  lambda_global <- (N[length(N)] / N[1])^(1/(nb_years-1))

  return(list(real_traj = n, obs_data = y, 
              lambda_global = lambda_global,
              lambda_i = lambda_i, growth_i = growth_i))
  
  
} # End function "simul"
######################################################-


make_warnings <- function(heterog_attrac_colo, site_fidelity){
  
  if(heterog_attrac_colo > 1) 
    print("Warning : Max value of 'heterog_attrac_colo' allowed is 1. It has thus been set to 1")
  
  if(heterog_attrac_colo < 0) 
    print("Warning : Min value of 'heterog_attrac_colo' allowed is 0. It has thus been set to 0")
  
  if(site_fidelity > 1)
    print("Warning : Max value of 'site_fidelity' allowed is 1. It has thus been set to 1")
  
  if(site_fidelity < 0)
    print("Warning : Min value of 'site_fidelity' allowed is 0. It has thus been set to 0")
  
} # End function "make_warnings"

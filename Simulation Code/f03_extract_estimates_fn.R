#############################################################################-
## Extract results/estimates 
extract_estimates <- function(out){
  
  # Outputs for "n_sc0": rows = iterations ; columns = parameters
  sel <- which((colnames(out$chain1) %>% substr(.,1,1)) == "n")
  all_chains <- rbind(out$chain1[,sel], out$chain2[,sel], out$chain3[,sel])
  
  est_n <- uci_n <- lci_n <- matrix(NA, nrow = nrow(y), ncol = ncol(y))
  est_n[] <- colMeans(all_chains)
  lci_n[] <- apply(all_chains, 2, quantile, probs = 0.025)
  uci_n[] <- apply(all_chains, 2, quantile, probs = 0.975)
  
  # Outputs for "growth_i"
  sel <- which(colnames(out$chain1) %>% substr(.,1,8) == "growth_i")
  all_chains <- rbind(out$chain1[,sel], out$chain2[,sel], out$chain3[,sel])
  
  est_growth_i <- colMeans(all_chains, na.rm = TRUE)
  lci_growth_i <- apply(all_chains, 2, quantile, probs = 0.025, na.rm = TRUE)
  uci_growth_i <- apply(all_chains, 2, quantile, probs = 0.975, na.rm = TRUE)
  
  # Outputs for "lambda_G"
  sel <- which(colnames(out$chain1) == "lambda_G")
  all_chains <- c(out$chain1[,sel], out$chain2[,sel], out$chain3[,sel])
  
  est_lambda_G <- mean(all_chains)
  lci_lambda_G <- quantile(all_chains, probs = 0.025)
  uci_lambda_G <- quantile(all_chains, probs = 0.975)
  
  return(
    list(est_n = est_n, lci_n = lci_n, uci_n = uci_n,
              est_lambda_G = est_lambda_G, lci_lambda_G = lci_lambda_G, uci_lambda_G = uci_lambda_G,
              est_growth_i = est_growth_i, lci_growth_i = lci_growth_i, uci_growth_i = uci_growth_i
         )
    )
  

  
} # end function


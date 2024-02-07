## Performance metric #####
# Functions
get_rmse <- function(est, real) sqrt(mean((est-real)^2)) 
get_rel_rmse <- function(est, real) sqrt(mean(((est-real)/real)^2)) 
get_bias <- function(est, real) mean(est - real)
get_rel_bias <- function(est, real) mean((est - real)/real)

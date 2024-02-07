## Utility function
util_mat <- function(fill, att, site_fidelity){
  I <- length(att)
  A <- matrix(NA, nrow = I, ncol = I)
  diag(A) <- site_fidelity*att
  A[is.na(A)] <- fill
  
  sum((colSums(A) - att)^2) + sum((rowSums(A) - rep(1,I))^2)
} # end of function


################################################################################-
## Build the movement matrix function
build_move_mat <- function(I = 10, d = 0.5, site_fidelity = 0.75){
  
    ## Define (random) the attractiveness level of each group
    aa <- runif(I, 0.5-(d/2), 0.5+(d/2))
    att <- (aa/sum(aa))*I
    
    sf <- site_fidelity
    
    diag_A <- ((att/I)*(1-sf))+sf
    fill_A <- ((1 - diag_A[1]) * (att/sum(att[-1])))
    A <- matrix(fill_A, nrow = I, ncol = I, byrow = TRUE)
    diag(A) <- diag_A
    
  return(A)
} # end function

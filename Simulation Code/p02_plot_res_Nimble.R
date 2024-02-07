## Function to plot estimated trajectories #####
plot_res_n <- function(real_n, est_n, lci_n, uci_n, show_legend = TRUE, title){
  
  n <- real_n
  
  ## Colors
  group_col <- c(brewer.pal(8,"Set1"), brewer.pal(8,"Set2"))
  
  ## dimensions
  ii <- 1:nrow(est_n)
  years <- 1:ncol(n)
  
  ## Define ylims
  ymin <- 0 
  ymax <- max(uci_n[ii,], real_n)
  
  
  # Estimates (IPM, MCMC)
  par(mar = c(6,6,4,1), cex.axis = 1.5, cex.lab = 2, cex.main = 2.5)
  plot(x = years, y = est_n[1,], 'n', ylim = c(ymin, ymax),
       ylab = "Effectifs",
       xlab = "Année",
       main = title)
  
  
  ## Colors
  CI_col <- rgb(red = col2rgb(group_col)["red",], 
                green = col2rgb(group_col)["green",],
                blue = col2rgb(group_col)["blue",],
                maxColorValue = 255, alpha = 100)
  
  ## Estimated Trends (average)
  for(i in ii){
    points(x = years, y = est_n[i,], 'l', pch = 1, lwd = 3, lty = 1, col = group_col[i])
  }
  
  ## CIs
  for(i in ii){
    polygon(x = c(years, rev(years)), 
            y = c(lci_n[i,], rev(uci_n[i,])), 
            col = CI_col[i], border = NA)
  }
  
  ## Plot real values
  for(i in 1:nrow(n)) points(x = 1:ncol(n), y = n[i,], 'p', pch = 1, cex = 2, col = group_col[i])
  
  ## Add legend
  if(show_legend) legend(x = 0, y = max(n), legend = c("réel", "estimé"), bty = "n", lwd = c(NA, 3), pch = c(1, NA), cex = 2)
  
} # end function





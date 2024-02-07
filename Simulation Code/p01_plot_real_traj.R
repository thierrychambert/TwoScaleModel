#### Plots ####
library("RColorBrewer")

## Colors
group_col <- c(brewer.pal(8,"Set1"), brewer.pal(8,"Set2"))

## Plot real values
par(mar = c(6,6,4,1), cex.axis = 1.5, cex.lab = 2, cex.main = 2.5)
plot(x = 1:nb_years, y = n[1,], 'n', ylim = c(0, max(n)), ylab = "Effectifs (réels)", xlab = "Année")
# real values
for(i in 1:nb_colo) points(x = 1:nb_years, y = n[i,], 'l', pch = 1, lwd = 5, lty = 1, col = group_col[i])

## Plot
if(show_obs){
  # observations
  for(i in 1:nb_colo) points(x = 1:nb_years, y = y[i,], 'p', pch = 19, cex = 1.5, col = group_col[i])
}

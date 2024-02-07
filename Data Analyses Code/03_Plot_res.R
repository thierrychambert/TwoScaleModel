## Plot results #####

## Choose how many colonies to show
if(!exists("ii")) ii <- 1:nrow(out$mean$n)
years <- as.numeric(gsub("X", "", names(data_count)))

## Define ylims
ymin <- 0
ymax <- max(c(out$q2.5$n[ii,], rev(out$q97.5$n[ii,])))

# Initiate the plot
par(mar = c(6,6,4,1), cex.axis = 1.5, cex.lab = 2, cex.main = 2.5)
plot(x = years, y = out$mean$n[1,], 'n', ylim = c(ymin, ymax),
     ylab = "Nombre de couples (comptés)",
     xlab = "Année",
     main = "Cormoran huppé"
)

## Define group colors 
use_col <- c("#4D4D4D", "#707070", "#898989", "#9E9E9E", "#AFAFAF", "#BFBFBF", 
             "#4DAF4A", "#377EB8", "#FF7F00", "#FFFF33", "#D7AE2E")


## CI Colors
CI_col <- rgb(red = col2rgb(use_col)["red",], 
              green = col2rgb(use_col)["green",],
              blue = col2rgb(use_col)["blue",],
              maxColorValue = 255, alpha = 100)

## Average Trends
for(i in ii){
  points(x = years, y = out$mean$n[i,], 'l', pch = 1, lwd = 5, lty = 1, col = use_col[i])
}

## CIs
for(i in ii){
  #  points(x = years, y = out$mean$n[i,], 'l', pch = 1, lwd = 5, lty = 1, col = use_col[i])
  polygon(x = c(years, rev(years)), 
          y = c(out$q2.5$n[i,], rev(out$q97.5$n[i,])), 
          col = CI_col[i], border = NA)
}

## Add observed counts
obs_n <- data_count[, paste0("X",2006:2016)]
obs_n[, ny0+(1:30)] <- NA
# for(i in ii)  points(x = years, y = obs_n[i,], "p", pch = 17, cex = 1.5,  col = use_col[i])

## growth_i
n <- out$mean$n
growth_i <- (n[,ncol(n)]/n[,1])^(1/(ncol(n)-1))

## Metapop size
N <- data.frame(year = 1:ncol(n))
N$metapop <- colSums(out$mean$n)

## Annual growth rates of each population
round(growth_i, 2)

## Metapopulation size, first and last year of projection
round(N[c(11, 41), ])

## Subpopulation sizes, first and last year of projection
out$mean$n[, c(11, 41)] %>% round(., 0)

## growth rate metapop
(N[ncol(n),2]/N[1,2])^(1/(ncol(n)-1))

## Save pop sizes estimates as tables
# For the classical model
# write.table(x = round(out$mean$n, 1), file = "./output_csv/subpop_sizes_M1.csv", row.names = FALSE, sep = ";")
# write.table(x = round(N), file = "./output_csv/metapop_sizes_M1.csv", row.names = FALSE, sep = ";")
# write.table(x = round(growth_i, 2), file = "./output_csv/growth_i_M1.csv", row.names = FALSE, sep = ";")

# For the two-scale model
## Save pop sizes estimates as tables
# write.table(x = round(out$mean$n, 1), file = "./output_csv/subpop_sizes_M2.csv", row.names = FALSE, sep = ";")
# write.table(x = round(out$mean$N), file = "./output_csv/metapop_sizes_M2.csv", row.names = FALSE, sep = ";")
# write.table(x = round(growth_i, 2), file = "./output_csv/growth_i_M2.csv", row.names = FALSE, sep = ";")

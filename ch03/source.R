##############################################################################
# Chapter 3 code
##############################################################################

## R code 3.1
PrPV <- 0.95
PrPM <- 0.01
PrV <- 0.001
PrP <- PrPV * PrV + PrPM * (1 - PrV)
print(PrVP <- (PrPV * PrV) / PrP)

## R code 3.2 - 3.5
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, length(p_grid))
likelihood <- dbinom(x = 6, size = 9, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 10000, replace = TRUE)
plot(samples)
library(rethinking)
dens(samples)

## R code 3.2 - 3.5
# add up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])

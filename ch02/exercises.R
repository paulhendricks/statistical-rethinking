##############################################################################
# Chapter 2 exercises
##############################################################################

## R code 2.3 - 2.5
# define main function
main <- function() {
  # compute likelihood at each value in grid
  likelihood <- dbinom(x = 6, size = 9, prob = p_grid)
  
  # compute product of likelihood and prior
  unstd_posterior <- likelihood * prior
  
  # standardize the posterior, so it sums to 1
  posterior <- unstd_posterior / sum(unstd_posterior)
  
  # plot
  par(mfrow=c(1,3))
  plot(p_grid, prior, type = "b")
  plot(p_grid, likelihood, type = "b")
  plot(p_grid, posterior, type = "b")
  
}

n <- 5
for (i in 1:n) {
  
  # define grid
  p_grid <- seq(from = 0, to = 1, length.out = n)
  
  # define prior
  prior <- rep(1, length(p_grid))
  main()
  
  # define prior
  prior <- ifelse(p_grid < 0.5, 0, 1)
  main()
  
  # define prior
  prior <- exp(-5 * abs(p_grid - 0.5))
  main()
}

## R code 2.6 - 2.7
library(rethinking)
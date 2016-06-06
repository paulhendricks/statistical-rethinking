##############################################################################
# Chapter 4 code
##############################################################################

## R code 4.1 - 4.5
pos <- replicate(1000, sum(runif(16, -1, 1)))
hist(pos)
plot(density(pos))

prod(1 + runif(12, 0, 0.1))
growth <- replicate(10000, prod(1 + runif(12, 0, 0.1)))
dens(growth, norm.comp = TRUE)

big <- replicate(10000, prod(1 + runif(12, 0, 0.5)))
dens(big, norm.comp = TRUE)
small <- replicate(10000, prod(1 + runif(12, 0, 0.01)))
dens(small, norm.comp = TRUE)
log.big <- replicate(10000, log(prod(1 + runif(12, 0, 0.5))))
dens(log.big, norm.comp = TRUE)

## R code 4.6
w <- 6; n <- 9;
p_grid <- seq(from = 0, to = 1, length.out = 100)
posterior <- dbinom(w, n, p_grid) * dunif(p_grid, 0, 1)
posterior <- posterior / sum(posterior)

## R code 4.7 - 4.

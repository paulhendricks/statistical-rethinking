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
library(rethinking)
data("Howell1")
d <- Howell1
str(d)
d$height
d2 <- d[d$age > 18, ]
curve(dnorm(x, 178, 20), from = 100, to = 250)
curve(dunif(x, 0, 50), from = -10, to = 60)
sample_mu <- rnorm(1e4, 178, 20)
sample_sigma <- runif(1e4, 0, 50)
prior_h <- rnorm(1e4, sample_mu, sample_sigma)
dens(prior_h)

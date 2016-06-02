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

## R code 3.6 - 3.19
# add up posterior probability where p < 0.5
sum(posterior[p_grid < 0.5])
sum(samples < 0.5) / 1e4
sum(0.5 < samples & samples < 0.75) / 1e4
quantile(samples, 0.8)
quantile(samples, c(0.1, 0.9))
p_grid <- seq(from = 0, to = 1, length.out = 1000)
prior <- rep(1, length(p_grid))
likelihood <- dbinom(x = 3, size = 3, prob = p_grid)
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
samples <- sample(p_grid, prob = posterior, size = 10000, replace = TRUE)
PI(samples, prob = 0.5)
HPDI(samples, prob = 0.5)
p_grid[which.max(posterior)]
chainmode(samples, adj = 0.01)
mean(samples)
median(samples)
sum(posterior * abs(0.5 - p_grid))
loss <- sapply(p_grid, function(d) sum(posterior * abs(d - p_grid)))
p_grid[which.min(loss)]

## R code 3.20 - 3.24
dbinom(0:2, size = 2, prob = 0.7)
rbinom(1, size = 2, prob = 0.7)
rbinom(10, size = 2, prob = 0.7)
dummy_w <- rbinom(1e5, size = 2, prob = 0.7)
table(dummy_w) / 1e5

dummy_w <- rbinom(1e5, size = 9, prob = 0.7)
simplehist(dummy_w, xlab = "dummy water count")

## R code 3.25 - 3.26
w <- rbinom(1e4, size = 9, prob = 0.6)
simplehist(w)

w <- rbinom(1e4, size = 9, prob = samples)
simplehist(w)

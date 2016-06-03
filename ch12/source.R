##############################################################################
# Chapter 12 code
##############################################################################

## R code 12.1 - 12.4
library(rethinking)
data("reedfrogs")
d <- reedfrogs
str(d)

d$tank <- 1:nrow(d)

m12.1 <- map2stan(
  alist(
    surv ~ dbinom(density, p), 
    logit(p) <- a_tank[tank], 
    a_tank[tank] ~ dnorm(0, 5)
  ), 
  data = d
)

precis(m12.1, depth = 2)
logistic(precis(m12.1, depth = 2)@output[, "Mean"])

m12.2 <- map2stan(
  alist(
    surv ~ dbinom(density, p), 
    logit(p) <- a_tank[tank], 
    a_tank[tank] ~ dnorm(a, sigma), 
    a ~ dnorm(0, 1), 
    sigma <- dcauchy(0, 1)
  ), 
  data = d, iter = 4000, chains = 4
)
precis(m12.2, depth = 2)
compare(m12.1, m12.2)

## R code 12.5 - 12.6
post <- extract.samples(m12.2)

## R code 12.5 - 12.4


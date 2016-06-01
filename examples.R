data(cars)
flist <- alist(
  dist ~ dnorm(mu , sigma),
  mu <- a + b * speed,
  a ~ dnorm(0, 10), 
  b ~ dnorm(0, 10), 
  sigma ~ dcauchy(0, 1)
)
fit <- map(flist , start = list(a = 40, b = 0.1, sigma = 20) , data = cars)
precis(fit)

# regularized logistic regression example
y <- c( rep(0,10) , rep(1,10) )
x <- c( rep(-1,9) , rep(1,11) )

flist0 <- alist(
  y ~ dbinom( prob=p , size=1 ) ,
  logit(p) <- a + b*x
)

flist1 <- alist(
  y ~ dbinom( prob=p , size=1 ),
  logit(p) <- a + b*x ,
  c(a,b) ~ dnorm(0,10)
)

# without priors, same problem as:
# glm3a <- glm( y ~ x , family=binomial , data=list(y=y,x=x) )
fit3a <- map( flist0 , data=list(y=y,x=x) , start=list(a=0,b=0) )
precis(fit3a)

# now with regularization
fit3b <- map( flist1 , data=list(y=y,x=x) , start=list(a=0,b=0) )
precis(fit3b)

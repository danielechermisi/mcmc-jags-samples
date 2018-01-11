# rm(list=ls())
library(rjags)

# DATA #
data_expr <- expression(
    "n" <- 100,
    "Y" <- 20,
    "a" <- 1,
    "b" <- 1
)
e <- new.env()
eval(data_expr, e)
data <- as.list(e)

# INIT #
init_expr <- expression(
    "theta" <- 0.1
)
e1 <- new.env()
eval(init_expr, e1)
inits <- as.list(e1)

# MODEL #
model_string <- '
var
   n, Y, a, b, theta;
model{

  # Likelihood
  Y ~ dbinom(theta,n)
  
  # Prior
  theta ~ dbeta(a, b)
}
'

m <- jags.model(textConnection(model_string), data, inits, n.chains=2)
update(m, 1000)
x <- coda.samples(m, c("theta"), n.iter=10000)
summary(x)
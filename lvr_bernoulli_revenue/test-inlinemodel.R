
library(rjags)

# DATA #
data_expr <- expression(
    "n" <- 10, 
    "Y" <- c(NA,NA,1,0,0,1,0,1,0,0), 
    "fatturato" <- c(100,230,123,120,1231,100,230,1230,1200,123010), 
    "missing" <- rep(0, n), 
    missing[which(is.na(Y))] <- fatturato[which(is.na(Y))])
e <- new.env()
eval(data_expr, e)
data <- as.list(e)


# INIT #
init_expr <- expression(
    "p" <- 0.5
)
e1 <- new.env()
eval(init_expr, e1)
inits <- as.list(e1)

# MODEL #
model_string <- "#
var
   n, Y[n], p, fatturato[n], missing[n];
model{

  # Likelihood
  for( i in 1 : n) {
    Y[i] ~ dbern( p )
  }


  Sreso <- sum(p * missing)
  # Prior
  p ~ dunif( 0, 1 )
  # p ~ dbeta(5,n)
}"

# PARSE MODEL #
m <- jags.model(textConnection(model_string), data, inits, n.chains=2)
update(m, 1000)
x <- coda.samples(m, c("p", "Sreso"), n.iter=10000)
summary(x)
plot(x)

# plot beta prior
support <- seq(0, 1, .001)
hseq <- dbeta(support, 5, 5)
p <- ggplot2::qplot(x = support, y = hseq, geom = "line")


# Valore medio e Quantili
ci <- quantile(x[[1]][,1],  probs = c(0.16, 0.84), names = FALSE)
mean <- mean(x[[1]][,1])

cat('valor medio ', mean)
print('')
cat('Quantile al 16%', ci[0])
print('')
cat ('Quantile all 84%', ci[1])


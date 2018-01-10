data <- read.jagsdata("resi-data.R")
inits <- read.jagsdata("resi-init.R")
m <- jags.model("resi.bug", data, inits, n.chains=2)
update(m, 1000)
x <- coda.samples(m, c("p", "Sreso"), n.iter=10000)
summary(x)
plot(x)


# beta prior
support <- seq(0, 1, .001)
hseq <- dbeta(support, 5, 5)
p <- ggplot2::qplot(x = support, y = hseq, geom = "line")
library('rjags')
data <- read.jagsdata("resi-data.R")
inits <- read.jagsdata("resi-init.R")
m <- jags.model("resi.bug", data, inits, n.chains=2)
update(m, 1000)
x <- coda.samples(m, c("p", "theta","pc", "thetac"), n.iter=10000)
summary(x)
plot(x)

# beta prior
support <- seq(0, 1, .001)
hseq <- dbeta(support, 5, 5)
p <- ggplot2::qplot(x = support, y = hseq, geom = "line")

# posterior importo reso
supporto_importo <- seq(min(data$V), max(data$V), 100)
campionamento_theta <- do.call(rbind.data.frame, x)$theta
valor_medio_theta <- mean(campionamento_theta)
distribuzione_parametro_importo <- dexp(supporto_importo, valor_medio_theta)
p <- ggplot2::qplot(x = supporto_importo, y = distribuzione_parametro_importo, geom = "line")

# mc simulation

eval<-function() {sum(ifelse(runif(min = 0, max=1, n=length(data$V))<=valor_medio_theta, data$V,0))} 
r<-replicate(100000,eval()) 
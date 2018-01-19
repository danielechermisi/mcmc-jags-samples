library('rjags')
data <- read.jagsdata("resi-data.R")
inits <- read.jagsdata("resi-init.R")
m <- jags.model("resi.bug", data, inits, n.chains=2)
update(m, 1000)
x <- coda.samples(m, c("p", "theta"), n.iter=10000)
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

#posterior probabilita' reso
campionamento_p <- do.call(rbind.data.frame, x)$p
valor_medio_p <- mean(campionamento_p)


# mc simulation
eval<-function() {sum(ifelse(runif(min = 0, max=1, n=length(data$V))<=valor_medio_p, data$V,0))} 
r<-replicate(100000,eval()) 



# La distribuzione a posteriori del parametro ordinato medio a confronto con esito simulazione montecarlo
rhist <- hist(r)
rhist_norm <- rhist$counts / sum(rhist$counts)
rhist$counts = rhist_norm


plot(rhist$breaks[-length(rhist$breaks)] + rhist$breaks[2]/2, rhist_norm, xlim=c(0,1400))
par(new=TRUE)
dpi_norm = distribuzione_parametro_importo# / sum(distribuzione_parametro_importo);
plot(supporto_importo, dpi_norm, axes=FALSE, ylim=c(0,1), col='red',xlim=c(0,1400))


#mc simulation using sampled posterior
max = length(data$V)
x_p = do.call(rbind.data.frame, x)$p;
spltted_posterior_sample = split(x_p, ceiling(x_p/max))
spltted_posterior_sample = split(x_p, ceiling(seq_along(x_p)/max))
spltted_posterior_sample <- spltted_posterior_sample[-length(spltted_posterior_sample)]

r_posterior <- seq(from= 0, to= 0, length.out = length(spltted_posterior_sample))
#r_posterior <- vector('list',length=length(spltted_posterior_sample))
for (ii in 1:length(spltted_posterior_sample)) {
    cat(ii, '\n')
    r_posterior[ii] = sum(ifelse(runif(min = 0, max=1, n=length(data$V))<=spltted_posterior_sample[[ii]] , data$V,0))
}

dev.off()
hist(r, freq=F,  xlim = c(0, 2000), ylim = c(0,0.0015))
par(new=TRUE)
hist(r_posterior, freq=F, xlim = c(0, 2000), ylim = c(0,0.0015), c='red'))
#rm(list=ls())
# http://bayesfactorpcl.r-forge.r-project.org/
# null: pr = 3 / 4
# alternative: the log-odds corresponding to pr, denoted ω=log(pr/(1−pr)),
# has a logistic distribution centered on the log-odds corresponding to the null 
# value pr (denoted ω0=log(pr/(1−pr))
k <- 682
n <- 682 + 243
p <- 3 / 4
bf = proportionBF( k, n, p = p)
1 / bf

# grafico della probabilita' a posteriori nel caso dell'ipotesi null
chains = posterior(bf, iterations = 10000)
plot(chains[,"p"], main = "Posterior of true probability\nof return")

plot(chains[,"logodds"], main = "Posterior of log-odds probability\nof return")


# versione frequentista del test binomiale
binom.test(k, n, p = p)

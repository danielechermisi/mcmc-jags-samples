# Intervallo di confidenza per la probabilita' del reso in caso di prior uniforme e prove bernoulliane
# https://en.wikipedia.org/wiki/Checking_whether_a_coin_is_fair
# numero resi
k <- 23000
# numero ordini
n <- 507000
alpha <- k + 1
beta <- n - k + 1
# intervalli di confidenza
ci <- qbeta(c(0.16, 0.84), alpha, beta);

# valor medio +- deviazione standard
mean <- alpha / ( alpha + beta )
stdev <- sqrt( (alpha * beta)/ ( (alpha + beta)^2 *(alpha + beta + 1) ))
mmin <- mean - stdev
mmax <- mean + stdev

cat('valor medio ', mean)
print('')
cat('Valore medio meno deviazione standard', mmin)
print('')
cat ('Valore medio piu deviazione standard', mmax)
print('')
cat  ('Quantili', ci)
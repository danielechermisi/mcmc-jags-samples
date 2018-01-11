# https://www.evanmiller.org/bayesian-ab-testing.html
# FORMA CHIUSA IN CASO DI PRIOR NON INFORMATIVE
# probability_B_beats_A = p(cr_B > cr_A)
# beta(a_A, b_A)

probability_B_beats_A <- function(a_A, b_A, a_B, b_B){
  total <- 0
	for (i in 0:(a_B - 1)) {
	    total <- total + exp(lbeta(a_A+i, b_B + b_A) - log(b_B+i) - lbeta(1+i, b_B) - lbeta(a_A, b_A))
	    #print(i)
	}
  return(total)
}


N_A <- 7017
N_B = 31314
c_A = 0.024 * N_A
c_B = 0.027 * N_B
a_A <- c_A +1
b_A <- N_A - c_A + 1
a_B <- c_B + 1
b_B <- N_B - c_B + 1
probability_B_beats_A(a_A,b_A,a_B,b_B)


N_A <- 100
N_B = 100
c_A = 10
c_B = 20
a_A <- c_A +1
b_A <- N_A - c_A + 1
a_B <- c_B + 1
b_B <- N_B - c_B + 1
probability_B_beats_A(a_A,b_A,a_B,b_B)
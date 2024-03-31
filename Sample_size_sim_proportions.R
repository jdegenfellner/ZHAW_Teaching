# Simulation sample sizes for estimation of porportions in finite samples:

N <- 9000 # Population sample size
n <- 350 # number of people answering the survey
p <- 0.35 # true but unknown proportion
n_sim <- 10000 # number of simulation run do determine CI

#x_sim <- rhyper(n, p*N, N-p*N, n)
#hist(x_sim/n)
#quantile(x_sim/n, probs = c(0.025, 0.975))

true_pop <- c(rep(1, N*p),rep(0,N-N*p)) # true population
length(true_pop) # N*p 1's N-N*p 0's


estimates_vec <- rep(NA, n_sim)

for(i in 1:n_sim){
  x_sample <- sample(true_pop, n, replace = FALSE)
  estimates_vec[i] <- sum(x_sample)/n # estimate of true but unknown proportion
}

hist(estimates_vec)
quantile(estimates_vec, probs = c(0.025, 0.975))
# Hence, when sampling n out of N people (representatively) with a true
# but unknown proportion p, we have a precision of +/- %5 (n=350, N=9000,p=0.35).
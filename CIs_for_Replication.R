set.seed(123)

nsim  <- 50000   # number of simulations
n     <- 30      # sample size in each study
mu    <- 0       # true mean
sigma <- 1       # true SD

count_in_CI <- 0

for(i in 1:nsim){
  
  ## First study
  x1  <- rnorm(n, mu, sigma)
  m1  <- mean(x1)
  se1 <- sd(x1) / sqrt(n)
  
  # 95% CI of the first study
  ci_lower <- m1 - qt(0.975, df = n-1)*se1
  ci_upper <- m1 + qt(0.975, df = n-1)*se1
  
  ## Replication study
  x2  <- rnorm(n, mu, sigma)
  m2  <- mean(x2)
  
  # Check whether replication mean falls inside the CI of the first study
  count_in_CI <- count_in_CI + (m2 >= ci_lower & m2 <= ci_upper)
}

# Proportion of replication means inside the original 95% CI
count_in_CI / nsim
# 0.84144


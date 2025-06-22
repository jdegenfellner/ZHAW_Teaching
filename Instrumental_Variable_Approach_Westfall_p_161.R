# Instrumental Variable Approach,
# Westfall p. 161-166

# Z -> X -> Y

# We are interested in the causal effect of X on Y.

n_sim <- 1000
correlation_strength <- 0.6
Sigma <- matrix(c(1^2, correlation_strength * 1 * 1,
                  correlation_strength * 1*1, 1^2), 2, 2)
# Correlation was verified via simulation.

data <- MASS::mvrnorm(n = n_sim, 
                      mu = c(0, 0), 
                      Sigma = Sigma)

Z <- data[, 1]
X <- data[, 2]

plot(Z, X, main = "Z vs X", xlab = "Z", ylab = "X")

Y <- 1 + 2*X + rnorm(n_sim)

# Instrumental variable estimator for gamma_1:
cov(Z, Y)/cov(Z,X)


# look at different n_sim's:--------

library(MASS)

# Funktion zur IV-Schätzung
iv_estimate <- function(n_sim, correlation_strength = 0.6, true_beta = 2) {
  Sigma <- matrix(c(1, correlation_strength,
                    correlation_strength, 1), nrow = 2)
  
  data <- MASS::mvrnorm(n = n_sim, mu = c(0, 0), Sigma = Sigma)
  Z <- data[, 1]
  X <- data[, 2]
  Y <- 1 + true_beta * X + rnorm(n_sim)
  
  iv_est <- cov(Z, Y) / cov(Z, X)
  return(iv_est)
}

# Replikationen
replicate_iv <- function(n_sim, n_rep = 100) {
  mean(replicate(n_rep, iv_estimate(n_sim)))
}

# Verschiedene Stichprobengrößen testen
sample_sizes <- c(10, 50, 100, 250, 500, 750, 1000)
iv_means <- sapply(sample_sizes, replicate_iv)

# In Data Frame umwandeln
results_df <- data.frame(
  SampleSize = sample_sizes,
  Mean_IV_Estimate = iv_means
)

print(results_df)

# looks correct.
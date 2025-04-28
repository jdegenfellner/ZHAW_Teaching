# Sample size calculation via simulation
# for Correlation coefficient

# Jan 25

library(pacman)
p_load(tidyverse, MASS)

# We assume that the relationship between the variables X and Y
# can be reasonable well described using a linear function.

# One way to search is to demand:--------
# The true but unknown correlation should lie within the interval
# [rho-delta, rho+delta]

# Let's assume, the data (X,Y) comes from a bivariate normal distribution
# (can be disputed of course)

# _Simulate data---------
# Parameters
n <- 100
mu <- c(0, 0)         # Means of the two variables
sigma1 <- 1           # Standard deviation of the first variable
sigma2 <- 2           # Standard deviation of the second variable
rho <- 0.5            # Correlation coefficient
delta <- 0.1         # Precision with which we want to know the correlation

# Covariance matrix
Sigma <- matrix(c(sigma1^2, rho * sigma1 * sigma2,
                  rho * sigma1 * sigma2, sigma2^2),
                nrow = 2)

# Simulate data
set.seed(123)         # For reproducibility
data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
data_df <- data.frame(X = data[, 1], Y = data[, 2])
head(data_df)
plot(data_df$X, data_df$Y, main = "Bivariate Normal Distribution",
     xlab = "X", ylab = "Y", pch = 19, col = rgb(0, 0, 1, 0.5))

cor(data_df$X, data_df$Y)

# _Verify, if - on average - rho is correct:--------
n_sim <- 500
rho_est <- numeric(n_sim)
for (i in 1:n_sim) {
  data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
  data_df <- data.frame(X = data[, 1], Y = data[, 2])
  rho_est[i] <- cor(data_df$X, data_df$Y)
}
hist(rho_est, main = "Histogram of estimated rho",
     xlab = "Estimated rho", col = "lightblue")
mean(rho_est) # seems to be OK.



# _How often is the true rho within the interval [rho-delta, rho+delta]?--------
# vary rho and n
func_perc_included <- function(n, rho, delta) {
  included_in_interval <- numeric(n_sim)
  Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)  # Fixed SD = 1
  for (i in 1:n_sim) {
    data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
    rho_est <- cor(data[, 1], data[, 2])
    included_in_interval[i] <- ifelse(rho_est >= rho - delta & rho_est <= rho + delta, 1, 0)
  }
  return(mean(included_in_interval))
}

n_seq <- seq(20, 120, by = 10)
rhos <- c(0.5, 0.7, 0.8)
results <- sapply(rhos, function(rho) {
  sapply(n_seq, function(n) func_perc_included(n, rho, delta))
})

# Plot Results
plot(NULL, xlim = range(n_seq), ylim = c(0, 1), 
     xlab = "Sample Size (n)", ylab = "Percentage Included", 
     main = paste("Percentage of Correlations Within Interval\n(delta =", delta, ")"))
colors <- c("red", "blue", "green")
for (i in seq_along(rhos)) {
  lines(n_seq, results[, i], type = "b", col = colors[i], pch = 19, lty = 1)
}
legend("bottomright", legend = paste("rho =", rhos), col = colors, pch = 19, lty = 1)

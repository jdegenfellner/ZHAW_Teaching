# From: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Estimation_standard_deviation_alternative_Fisher_1922.R

# Fisher, R. (1922), “On the Mathematical Foundations of Theoretical Statistics,
# ” Philosophical Transaction of Royal Society A, 222, 309–368.

# Load necessary library
library(MASS)

# Set seed for reproducibility
set.seed(42)

# Function to calculate mean error (σ1)
mean_error <- function(x) {
  n <- length(x)
  x_median <- median(x)
  return((1 / n) * sqrt(pi / 2) * sum(abs(x - x_median)))
}

# Function to calculate mean square error (σ2)
mean_square_error <- function(x) {
  n <- length(x)
  return(sqrt((1 / n) * sum((x - mean(x))^2)))
}

# Generate a normally distributed sample
n <- 1000
mu <- 0    # Mean of the normal distribution
sigma <- 1 # Standard deviation of the normal distribution
sample <- rnorm(n, mean = mu, sd = sigma)

# Calculate the estimators
sigma1 <- mean_error(sample)
sigma2 <- mean_square_error(sample)

# True population standard deviation
true_sigma <- sigma

# Output the results
cat("Mean Error (σ1):", sigma1, "\n")
cat("Mean Square Error (σ2):", sigma2, "\n")
cat("True Population Standard Deviation (σ):", true_sigma, "\n")

# Check consistency
cat("Is σ1 consistent with the true σ? ", all.equal(sigma1, true_sigma), "\n")
cat("Is σ2 consistent with the true σ? ", all.equal(sigma2, true_sigma), "\n")

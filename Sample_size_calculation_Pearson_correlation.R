# Sample size calculation via simulation
# for Correlation coefficient

# Jan 25

library(pacman)
p_load(tidyverse, MASS, tictoc)

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

# Simulate "zero" (i.e. low) correlation------------
simulate_correlation_in_interval <- function(n = 50, 
                                             lower_bound = -0.15, 
                                             upper_bound = 0.15, 
                                             n_sim = 1e5) {
  
  true_rhos <- runif(n_sim, min = lower_bound, max = upper_bound)
  inside_interval <- numeric(n_sim)
  
  mu <- c(0, 0)  # Mittelwerte
  
  for (i in seq_len(n_sim)) {
    rho <- true_rhos[i]
    Sigma <- matrix(c(1, rho, rho, 1), nrow = 2)
    data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
    rho_est <- cor(data[, 1], data[, 2])
    
    inside_interval[i] <- as.integer(rho_est >= lower_bound & 
                                       rho_est <= upper_bound)
  }
  hist(true_rhos)
  mean(inside_interval)  # Anteil der Korrelationen innerhalb der Grenzen
}

# fixed n:
set.seed(123)
tic() # start timer
simulate_correlation_in_interval(n = 100, 
                                 lower_bound = -0.3, 
                                 upper_bound = 0.3, 
                                 n_sim = 1e5)
toc() # ~4s


# multiple n:
n_values <- c(30, 50, 70, 90)

results <- numeric(length(n_values))

tic("Gesamtzeit")
for (i in seq_along(n_values)) {
  results[i] <- simulate_correlation_in_interval(n = n_values[i], 
                                                 lower_bound = -0.3, 
                                                 upper_bound = 0.3, 
                                                 n_sim = 1e5)
}
toc()

# In Dataframe umwandeln
results_df <- data.frame(
  Sample_Size = n_values,
  Proportion_Within_Bounds = results
)

print(results_df)

lower_bound <- -0.3
upper_bound <- 0.3

ggplot(results_df, aes(x = Sample_Size, y = Proportion_Within_Bounds)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 3, color = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = paste0("Anteil der geschätzten Korrelationen innerhalb [", 
                   lower_bound, ", ", upper_bound, "]"),
    x = "Stichprobengröße (n)",
    y = "Anteil innerhalb der Grenzen"
  ) +
  theme_minimal(base_size = 14)

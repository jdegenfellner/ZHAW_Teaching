# Estimating coverage probability of the confidence interval for the mean
# from the central limit theorem (CLT)

# GPT4 used.
library(pacman)
p_load(tidyverse)

estimate_coverage_probability <- function(distr, params, 
                                          sample_size, 
                                          num_simulations = 10000, 
                                          conf_level = 0.95) {
  coverage_count <- 0
  
  for (i in 1:num_simulations) {
    # Generate a sample
    sample <- do.call(distr, c(list(n = sample_size), params))
    # Calculate sample mean and standard error
    sample_mean <- mean(sample)
    sample_se <- sd(sample) / sqrt(sample_size)
    # Calculate confidence interval
    z <- qnorm(1 - (1 - conf_level) / 2)
    ci_lower <- sample_mean - z * sample_se
    ci_upper <- sample_mean + z * sample_se
    # Check if the CI contains the true mean (for standard distributions, mean is typically a parameter)
    if (ci_lower <= params$mean && ci_upper >= params$mean) {
      coverage_count <- coverage_count + 1
    }
  }
  
  # Estimate of coverage probability
  coverage_probability <- coverage_count / num_simulations
  return(coverage_probability)
}
# Example:
estimate_coverage_probability("rnorm", list(mean = 0, sd = 1), sample_size = 10)

# Look at different sample sizes:
sizes <- seq(from = 10, to = 200, by = 2)
df <- data.frame(sizes = sizes, 
                 cov_prob = sizes)

for(i in 1:length(sizes)){
  df$cov_prob[i] <- estimate_coverage_probability("rnorm", list(mean = 0, sd = 1), 
                                                  sample_size = sizes[i])
}

df %>% ggplot(aes(x=sizes, y=cov_prob)) + 
  geom_line() + 
  geom_smooth() +
  geom_hline(yintercept = 0.95)

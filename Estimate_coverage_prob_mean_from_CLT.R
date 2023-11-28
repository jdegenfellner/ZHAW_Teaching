# Estimating coverage probability of the confidence interval for the mean
# from the central limit theorem (CLT)

# in progress #

# GPT4 used.
library(pacman)
p_load(tidyverse)

estimate_coverage_probability <- function(distr, params, 
                                          sample_size, 
                                          num_simulations = 10000, 
                                          conf_level = 0.95) {
  coverage_count <- 0
  
  for (i in 1:num_simulations) {
    sample <- do.call(distr, c(list(n = sample_size), params))
    sample_mean <- mean(sample)
    sample_se <- sd(sample) / sqrt(sample_size) # standard error
    z <- qnorm(1 - (1 - conf_level) / 2) # 1.96 for 95%
    ci_lower <- sample_mean - z * sample_se
    ci_upper <- sample_mean + z * sample_se
    if (ci_lower <= params$mean && ci_upper >= params$mean) {
      coverage_count <- coverage_count + 1
    }
  }
  
  coverage_probability <- coverage_count / num_simulations
  return(coverage_probability)
}


# Example:----
estimate_coverage_probability("rnorm", list(mean = 0, sd = 1), sample_size = 10)

# Look at different sample sizes:----
# _1) Standard normal N(0,1)----
# Note that we would have an exact CI here (since exactly t-distributed)
sizes <- seq(from = 10, to = 200, by = 5)
df <- data.frame(sizes = sizes, 
                 cov_prob = sizes)

for(i in 1:length(sizes)){
  df$cov_prob[i] <- estimate_coverage_probability("rnorm", list(mean = 0, sd = 1), 
                                                  sample_size = sizes[i])
}

df %>% ggplot(aes(x = sizes, y = cov_prob)) + 
  geom_line() + 
  geom_smooth() +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") + 
  ggtitle("(Simulated) Coverage probability of the CI for the mean (from CLT)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Simulated coverage probability") + xlab("sample size (n)")
  

# _2) Gamma----
estimate_coverage_probability_gamma <- function(distr, params, sample_size, num_simulations = 10000, conf_level = 0.95) {
  coverage_count <- 0
  
  for (i in 1:num_simulations) {
    sample <- do.call(distr, c(list(n = sample_size), params))
    sample_mean <- mean(sample)
    sample_se <- sd(sample) / sqrt(sample_size)
    z <- qnorm(1 - (1 - conf_level) / 2)
    ci_lower <- sample_mean - z * sample_se
    ci_upper <- sample_mean + z * sample_se
    true_mean <- params$shape / params$rate
    if (ci_lower <= true_mean && ci_upper >= true_mean) {
      coverage_count <- coverage_count + 1
    }
  }
  
  coverage_probability <- coverage_count / num_simulations
  return(coverage_probability)
}

# Now try the function with the gamma distribution
estimate_coverage_probability_gamma("rgamma", list(shape = 2, rate = 1), sample_size = 10)


for(i in 1:length(sizes)){
  df$cov_prob[i] <- estimate_coverage_probability_gamma("rgamma", list(shape = 2, rate = 1), 
                                                  sample_size = sizes[i])
}

df %>% ggplot(aes(x = sizes, y = cov_prob)) + 
  geom_line() + 
  geom_smooth() +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "red") + 
  ggtitle("(Simulated) Coverage probability of the CI for the mean (from CLT)") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  ylab("Simulated coverage probability") + xlab("Sample size (n)")


# TODO
# use only one function for all distributions
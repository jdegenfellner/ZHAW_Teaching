library(pacman)
p_load(tidyverse)

# Set seed for reproducibility
#set.seed(123)

perform_t_test <- function(sample_size, true_mean = 0.0001) {
  data <- rnorm(sample_size, mean = true_mean, sd = 1)
  t_test_result <- t.test(data, mu = 0)
  return(t_test_result$p.value)
}

sample_sizes <- c(10, 30, 50, 100, 500, 1000, 5000, 10000, 50000, 100000)
p_values <- sapply(sample_sizes, perform_t_test)
results <- data.frame(Sample_Size = sample_sizes, 
                      P_Value = p_values)

ggplot(results, aes(x = Sample_Size, y = P_Value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(trans='log10') + 
  labs(title = "P-values from One-Sample t-tests with Increasing 
       Sample Sizes (True Mean = 0.0001)",
       x = "Sample Size",
       y = "P-Value (log scale)") +
  theme_minimal()

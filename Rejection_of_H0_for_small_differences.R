library(pacman)
p_load(tidyverse)

# https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Rejection_of_H0_for_small_differences.R

#set.seed(123)

true_mean_ <- 0.001

perform_t_test <- function(sample_size, true_mean = true_mean_) {
  data <- rnorm(sample_size, mean = true_mean_, sd = 1)
  t_test_result <- t.test(data, mu = 0)
  return(t_test_result$p.value)
}

sample_sizes <- c(10, 30, 50, 100, 500, 1000, 5000, 10000, 50000, 100000)
p_values <- sapply(sample_sizes, perform_t_test, true_mean = true_mean_)
results <- data.frame(Sample_Size = sample_sizes, 
                      P_Value = p_values)
results
ggplot(results, aes(x = Sample_Size, y = P_Value)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(trans='log10') + 
  labs(title = paste0("P-values from One-Sample t-tests with Increasing 
       Sample Sizes (True Mean = ", true_mean_,")"),
       x = "Sample Size (log scale)",
       y = "P-Value") +
  theme_minimal() +
  geom_hline(yintercept = 0.05, color="red") + 
  geom_hline(yintercept = 0.01, color="red")

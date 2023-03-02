# Instruction to ChatGPT:
# "write an R program that shows the quality of the 
# confidence intervals from the central limit theorem.  
# speficially, how often is the true mean included in the 
# confidence interval depending on the sample size?"

library(ggplot2)

# Generate random samples from a normal distribution
set.seed(123)
sample_size <- c(5, 10, 15, 20, 100)
samples_per_size <- 1000
true_mean <- 3
true_sd <- 3

results <- data.frame(matrix(ncol = samples_per_size + 1, nrow = length(sample_size)))
colnames(results) <- c("SampleSize", paste0("Sample", 1:samples_per_size))

for (i in 1:length(sample_size)) {
  n <- sample_size[i]
  results[i, 1] <- n
  for (j in 1:samples_per_size) {
    #sample <- rnorm(n, mean = true_mean, sd = true_sd) # True distribution is normal
    sample <- rpois(n = n, lambda = true_mean) # Try other distribution
    mean_estimate <- mean(sample)
    sd_estimate <- sd(sample)
    ci <- mean_estimate + c(-1, 1) * qnorm(0.975) * sd_estimate / sqrt(n) # CTL approx. 95% confidence interval
    results[i, j + 1] <- (ci[1] <= true_mean & true_mean <= ci[2])
  }
}

# Calculate the proportion of times the true mean was included in the CI
proportions <- apply(results[, -1], 1, mean)
proportions_df <- data.frame(SampleSize = sample_size, Proportion = proportions)

proportions_df

# Plot the results
ggplot(proportions_df, aes(x = SampleSize, y = Proportion)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = paste0(round(100 * Proportion, 2), "%")), position = position_dodge(width = 0.9), vjust = -0.25, size = 2) +
  xlab("Sample size") + ylab("Proportion of times true mean was included in CI") +
  ggtitle("Quality of Confidence Intervals from the Central Limit Theorem") +
  theme_classic()

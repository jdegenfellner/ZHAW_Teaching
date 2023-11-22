# ad Standardfehler - vergleiche zwei Schaetzer für den Erwartungswert.
# Wie gross ist jeweils die Streuung?

library(pacman)
p_load(tidyverse)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Generiere eine sample aus einer Zufallszahl (wir wissen hier, wie diese----
# verteilt ist):
n <- 10000
x <- rnorm(n, mean = 23.4, sd = 5.6)
df <- data.frame(values = x)

p2 <- ggplot(df, aes(x = values)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.7, color="darkgrey") +
  geom_density(aes(y = after_stat(density)), color = "blue", size=1) +
  geom_boxplot(aes(y = -0.01, x = values), width = 0.02, position = position_nudge(y = -0.00)) +
  geom_point(aes(y = -0.01), 
             position = position_jitter(width = 0.002, height = 0.01), 
             size = 1, alpha = 0.05) +
  ggtitle("Histogram with density plot and boxplot below") +
  theme(plot.title = element_text(hjust = 0.5))
p2

# Wir wollen den wahren Mittelwert 23.4 schaetzen:----

# Schaetzer 1: arithmetisches Mittel
(x1 <- mean(x))
abs(x1-23.4) # z.B. 0.0551746

# Schaetzer 2: Mittelwert aus groesster und kleinster Beobachtung
(x2 <- mean(c(min(x), max(x))))
abs(x2-23.4) # z.B. 0.5289857


# Das machen wir jetzt 1000 mal, damit wir einen Eindruck davon bekommen,
# wie gut die beiden Schaetzer sind.

# Set the seed for reproducibility
set.seed(0)

# Function to perform the estimation process
estimate_means <- function(n) {
  x <- rnorm(n, mean = 23.4, sd = 5.6)
  x1 <- mean(x)
  x2 <- mean(c(min(x), max(x)))
  return(c(x1, x2))
}

# Perform the process n_sim times and store the results
n_sim <- 1000
estimates <- replicate(n_sim, estimate_means(n))
estimates_df <- as.data.frame(t(estimates))
colnames(estimates_df) <- c("arithm_Mittel", "Mittelwert_Min_Max")

# Standardfehler aus Simulation:
(std_dev_x1 <- sd(estimates_df$arithm_Mittel))
# vgl. Standardfehler laut Formel se(\bar{X} = \frac{s}{\sqrt(n)})
sd(x)/sqrt(length(x)) # very close (hier wurde nur eine sample genommen)

(std_dev_x2 <- sd(estimates_df$Mittelwert_Min_Max))


# Convert the data frame to long format using pivot_longer from the tidyr package
estimates_long <- pivot_longer(estimates_df, cols = everything(), names_to = "Estimator", values_to = "Estimate")

# Plotting the histograms using ggplot2
ggplot(estimates_long, aes(x = Estimate, fill = Estimator)) +
  geom_histogram(aes(y = after_stat(density)), position = 'identity', alpha = 0.7, bins = 30) +
  facet_wrap(~Estimator, scales = 'free_y') +
  theme_minimal() +
  labs(title = paste0("n_sample = ",n,"; ","n_sim = ",n_sim), 
       x = "Estimated Value", 
       y = "Density") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = Inf, y = Inf, label = sprintf("Std.fehler arithm.Mittel: %0.2f", std_dev_x1),
           hjust = 1.1, vjust = 2, size = 3.5, color = "red") +
  annotate("text", x = Inf, y = Inf, label = sprintf("Std.fehler MinMax: %0.2f", std_dev_x2),
           hjust = 1.1, vjust = 1, size = 3.5, color = "blue") + 
  theme(plot.title = element_text(hjust = 0.5))


# Note, that one could expand this example to other estimators for other 
# parameters and other distributions


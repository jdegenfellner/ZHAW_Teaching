# Load necessary libraries
if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, gganimate)

# Parameters
num_trials <- 1000       # Number of coin tosses or experiments
true_probability <- 1/6  # True probability of the event (e.g., 1/6 for rolling a specific number on a die)
z_value <- qnorm(0.97)   # 94% confidence level, z_alpha/2 = 1.88

# Simulate a sequence of random experiments (e.g., biased coin flips)
set.seed(42)  # For reproducibility
outcomes <- rbinom(num_trials, 1, true_probability)  # 1 = event A (e.g., success), 0 = not A (failure)

# Calculate the relative frequency of A up to each trial
relative_frequencies <- cumsum(outcomes) / (1:num_trials)

# Calculate the 94% confidence intervals for each trial
conf_band_upper <- relative_frequencies + z_value * sqrt((relative_frequencies * (1 - relative_frequencies)) / (1:num_trials))
conf_band_lower <- relative_frequencies - z_value * sqrt((relative_frequencies * (1 - relative_frequencies)) / (1:num_trials))

# Ensure the confidence bounds do not exceed the interval [0, 1]
conf_band_upper <- pmin(conf_band_upper, 1)
conf_band_lower <- pmax(conf_band_lower, 0)

# Create a data frame for the animation, starting at trial 10
data <- data.frame(
  trial = 10:num_trials,
  rel_freq = relative_frequencies[10:num_trials],
  conf_upper = conf_band_upper[10:num_trials],
  conf_lower = conf_band_lower[10:num_trials]
)

# Create the ggplot object
p <- ggplot(data, aes(x = trial, y = rel_freq)) +
  geom_ribbon(aes(ymin = conf_lower, ymax = conf_upper), fill = "blue", alpha = 0.2) +  # Confidence band
  geom_line(size = 1) +
  geom_hline(yintercept = true_probability, color = "red", linetype = "dashed") +
  labs(
    title = "Law of Large Numbers: Stabilizing of Relative Frequency with 94% Confidence Band",
    subtitle = "Trial: {frame_time}",
    x = "Number of Trials",
    y = expression(Relative~Frequency~H(A) == n[A] / n)
  ) +
  theme_minimal() +
  transition_reveal(trial) +
  ease_aes('linear')

# Display the animation directly in the RStudio viewer
animate(p, nframes = 150, fps = 10, width = 1000, height = 800)
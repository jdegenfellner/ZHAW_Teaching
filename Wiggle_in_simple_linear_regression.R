library(ggplot2)
library(gganimate)
library(gifski)
library(dplyr)
library(tidyr)

# Set seed for reproducibility
set.seed(123)

# Define parameters for the true regression model
beta0 <- 5   # Intercept
beta1 <- 2   # Slope
sigma <- 1.5 # Standard deviation of residuals

# Generate x values
n <- 50
x_vals <- seq(-2, 2, length.out = n)

# Define number of frames for smooth wiggle animation
time_steps <- 100

# Create a data frame that contains all points for each time frame
sim_data <- expand_grid(x = x_vals, time = seq(1, time_steps, length.out = time_steps)) %>%
  group_by(x) %>%
  mutate(
    base_y = beta0 + beta1 * x,  # Fixed true regression line
    wiggle_effect = sin(time / 10 + x * 2) * sigma * 0.3,  # Each point wobbles independently
    noise = rnorm(n(), mean = 0, sd = sigma * 0.3),  # Small random noise for realistic movement
    y = base_y + wiggle_effect + noise  # Final wiggling y-values
  ) %>% 
  ungroup()

# Create animated plot
p <- ggplot(sim_data, aes(x = x, y = y, group = x)) +
  geom_abline(intercept = beta0, slope = beta1, color = "blue", size = 1.2) +  # True regression line (fixed)
  geom_point(color = "red", size = 2, alpha = 0.7) +
  labs(title = "Continuous Wiggle in Linear Regression",
       subtitle = "Time: {frame_time}",
       x = "X", y = "Y") +
  theme_minimal() +
  transition_time(time) +
  ease_aes('cubic-in-out') + 
  theme(plot.title = element_text(hjust = 0.5))

# Save the GIF
anim_save("continuous_wiggle_fixed_mean.gif", p, duration = 5, fps = 20, renderer = gifski_renderer())

# Display animation
p
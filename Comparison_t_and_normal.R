# Comparision t- and normal distribution:

# GPT4 used.
library(pacman)
p_load(tidyverse)

x_values <- seq(-4, 4, by = 0.01)

comparison_df <- data.frame(
  x = c(x_values, x_values, x_values),
  Density = c(dnorm(x_values), dt(x_values, df = 10), dt(x_values, df = 30)),
  Distribution = factor(rep(c("Normal", "t (df = 10)", "t (df = 30)"), 
                            each = length(x_values)))
)

ggplot(comparison_df, aes(x = x, y = Density, color = Distribution)) +
  geom_line() +
  scale_color_manual(values = c("black", "red", "blue")) +
  labs(title = "Comparison of Normal and t Distributions around n = 30",
       x = "Z/T value",
       y = "Density",
       color = "Distribution") +
  theme_minimal()

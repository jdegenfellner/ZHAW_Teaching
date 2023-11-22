# ad Standardfehler - vergleiche zwei Schaetzer für den Erwartungswert.
# Wie gross ist jeweils die Streuung?

library(pacman)
p_load(tidyverse)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Generiere eine sample aus einer Zufallszahl (wir wissen hier, wie diese
# verteilt ist):
x <- rnorm(10000, mean = 23.4, sd = 5.6)
df <- data.frame(values = x)

p2 <- ggplot(df, aes(x = values)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, alpha = 0.7) +
  geom_density(aes(y = after_stat(density)), color = "blue") +
  geom_boxplot(aes(y = -0.03, x = values), width = 0.01, position = position_nudge(y = -0.00)) +
  geom_point(aes(y = -0.03), position = position_jitter(width = 0.002, height = 0.01), size = 1, alpha=0.05) +
  ggtitle("Histogram with density plot and boxplot below") +
  theme(plot.title = element_text(hjust = 0.5))
p2

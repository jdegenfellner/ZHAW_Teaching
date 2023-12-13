# Distribution of:
# W in :
# https://de.wikipedia.org/wiki/Wilcoxon-Vorzeichen-Rang-Test

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages-----------------------------------------------------------------
library(pacman)
pacman::p_load(tidyverse)

shape <- 2
scale <- 2

x <- rgamma(1000, shape, scale)
y <- rgamma(1000, shape, scale)
data <- data.frame(Value = c(x, y), 
                   Group = factor(rep(c("X", "Y"), each = 1000)))
median_x <- median(x)
median_y <- median(y)

ggplot(data, aes(x = Value, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(xintercept = median_x, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_y, color = "red", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("blue", "red")) +
  ggtitle("Combined Histogram of Gamma Distributed Random Numbers") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        legend.title = element_blank())  # Center the title


W_results <- c()
n_sim <- 1000
n <- 1000
EW <- 1/4*n*(n+1)
VarW <- n*(n+1)*(2*n+1)/24
for(i in 1:n_sim){
  x <- rgamma(n, shape, scale)
  y <- rgamma(n, shape, scale)
  D <- (x - y) # differences
  R <- rank(abs(D)) # ordered abs. diffs
  Wplus <- sum(R[D > 0]) # stat1
  Wminus <- sum(R[D < 0]) # stat2
  W <- min(Wplus, Wminus)
  W_results <- append(W_results, W)
}
Z <- (W_results - EW)/sqrt(VarW)

ggplot(data.frame(Z=Z), aes(x=Z)) + 
  geom_histogram()

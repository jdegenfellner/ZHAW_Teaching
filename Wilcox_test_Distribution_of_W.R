# Distribution of:
# W in :
# https://de.wikipedia.org/wiki/Wilcoxon-Vorzeichen-Rang-Test

# in progress # 

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load packages-----------------------------------------------------------------
library(pacman)
pacman::p_load(tidyverse, tictoc)

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
n_sim <- 100000
n <- 1000
tic()
for(i in 1:n_sim){
  #x <- rgamma(n, shape, scale)
  #y <- rgamma(n, shape, scale)
  x <- rnorm(n, mean = 10, sd = 3) # H_0 is true.
  y <- rnorm(n, mean = 10, sd = 3)
  D <- (x - y) 
  R <- rank(abs(D))
  Wplus <- sum(R[D > 0]) 
  Wminus <- sum(R[D < 0]) 
  #W <- min(Wplus, Wminus) # this results in only negative Z
  W <- ifelse(runif(1)>0.5, Wminus, Wplus)
  W_results <- append(W_results, W)
}
toc() # n_sim <- 100000 24.025 sec elapsed
EW <- 1/4*n*(n+1)
VarW <- n*(n+1)*(2*n+1)/24
Z <- (W_results - EW)/sqrt(VarW)

ggplot(data.frame(Z=Z), aes(x=Z)) + 
  geom_histogram(aes(y = ..density..), fill = "blue", color = "black") + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 1) +
  ggtitle("Histogram with Standard Normal Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())


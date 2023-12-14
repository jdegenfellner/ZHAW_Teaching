# From: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Wilcox_test_Distribution_of_W.R
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
y <- rgamma(1000, shape, scale) # H_0 true, no location shift.
data <- data.frame(Value = c(x, y), 
                   Group = factor(rep(c("X", "Y"), each = 1000)))
median_x <- median(x)
median_y <- median(y)

ggplot(data, aes(x = Value, fill = Group)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  geom_vline(xintercept = median_x, color = "blue", linetype = "dashed", size = 1) +
  geom_vline(xintercept = median_y, color = "red", linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("blue", "red")) +
  ggtitle("H_0 is true") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 9),
        legend.title = element_blank())  # Center the title

W_results <- c()
n_sim <- 10000
n <- 1000
tic()
for(i in 1:n_sim){
  x <- rgamma(n, shape, scale)
  y <- rgamma(n, shape, scale)
  #x <- rnorm(n, mean = 10, sd = 3) # H_0 is true.
  #y <- rnorm(n, mean = 10, sd = 3)
  D <- (x - y) 
  R <- rank(abs(D))
  Wplus <- sum(R[D > 0]) 
  Wminus <- sum(R[D < 0]) 
  #W <- min(Wplus, Wminus) # this results in only negative Z!
  #W <- ifelse(runif(1)>0.5, Wminus, Wplus)
  W <- Wplus # works too, see https://epub.ub.uni-muenchen.de/25569/1/BA_Steinherr.pdf
  #W <- Wminus # works too
  W_results <- append(W_results, W)
}
toc() # n_sim <- 100000 24.025 sec elapsed
EW <- 1/4*n*(n+1)
VarW <- n*(n+1)*(2*n+1)/24
Z <- (W_results - EW)/sqrt(VarW) # appr. N(0,1)

ggplot(data.frame(Z=Z), aes(x=Z)) + 
  geom_histogram(aes(y = ..density..), fill = "blue", color = "black") + 
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 1) +
  ggtitle("Standardized W") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank()) +
  annotate("text", x = Inf, y = Inf, label = paste("Mean(Z):", 
           round(mean(Z), 6), "\nSD(Z):", round(sd(Z), 6)),
           hjust = 1.1, vjust = 1.1, size = 3, color = "black")
# -> It seems that symmetry does not play a major role in terms of distribution


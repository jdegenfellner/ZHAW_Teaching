# Fisher z-transformation for correlation:
# https://en.wikipedia.org/wiki/Fisher_transformation

# "If (X, Y) has a bivariate normal distribution with correlation ρ and the
# pairs (Xi, Yi) are independent and identically distributed, then z is
# approximately normally distributed with mean 1/2*log((1+rho)/(1-rho))"

library(pacman)
p_load(tidyverse, MASS, moments)

fisher_transformation <- function(r) {
  0.5 * log((1 + r) / (1 - r)) # =atanh()
}

n <- 1000
n_sim <- 1000
mu <- c(0, 0)
rho <- 0.98
sigma <- matrix(c(1, rho, rho, 1), nrow = 2) # Korrelationsmatrix mit r = 0.8

cors_orig <- rep(NA, n)
cors_fisher_transform <- rep(NA, n)

for(i in 1:n_sim){
  data <- as.data.frame( mvrnorm(n, mu, sigma) )
  colnames(data) <- c("X", "Y")
  cors_orig[i] <- cor(data$X, data$Y)
  cors_fisher_transform[i] <- fisher_transformation(cors_orig[i])
}

df <- data.frame(cors_orig = cors_orig,
                 cors_fisher_transform = cors_fisher_transform)


shapiro.test(cors_fisher_transform)
hist(cors_fisher_transform) # not too bad.

hist(cors_orig) # skewed??
summary(cors_orig) # do not look skewed?

skewness(cors_orig) # not zero (without test)
skewness(cors_fisher_transform) # near zero

# check: mean = 
mean(cors_fisher_transform)
1/2*log((1+rho)/(1-rho)) # spot-on!

ggplot(df) +
  geom_histogram(aes(x = cors_orig, y = after_stat(density)), 
                 binwidth = 0.05, fill = "blue", alpha = 0.6) +
  geom_density(aes(x = cors_orig), color = "blue") +
  geom_histogram(aes(x = cors_fisher_transform, y = -after_stat(density)), 
                 binwidth = 0.2, fill = "red", alpha = 0.6) +
  geom_density(aes(x = cors_fisher_transform), color = "red") +
  labs(title = "Distribution of original correlations and Fisher-transformed correlations",
       x = "",
       y = "") +
  theme_minimal() +
  scale_y_continuous(labels = abs)

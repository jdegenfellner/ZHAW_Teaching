# Fisher z-transformation for correlation:
# https://en.wikipedia.org/wiki/Fisher_transformation

library(pacman)
p_load(tidyverse, MASS)

# Fisher-Transformation Funktion
fisher_transformation <- function(r) {
  0.5 * log((1 + r) / (1 - r)) # =atanh()
}

n <- 1000
mu <- c(0, 0)
sigma <- matrix(c(1, 0.8, 0.8, 1), nrow = 2) # Korrelationsmatrix mit r = 0.8
cors_orig <- rep(NA, n)
cors_fisher_transform <- rep(NA, n)

for(i in 1:n){
  data <- as.data.frame( mvrnorm(n, mu, sigma) )
  colnames(data) <- c("X", "Y")
  cors_orig[i] <- cor(data$X, data$Y)
  cors_fisher_transform[i] <- fisher_transformation(cors_orig[i])
}

df <- data.frame(cors_orig = cors_orig,
                 cors_fisher_transform = cors_fisher_transform)


ggplot() +
  geom_histogram(data = df, aes(x = cors_orig, y = ..density..), binwidth = 0.05, fill = "blue", alpha = 0.6) +
  geom_density(data = df, aes(x = cors_orig), color = "blue") +
  geom_histogram(data = df, aes(x = cors_fisher_transform, y = -..density..), binwidth = 0.2, fill = "red", alpha = 0.6) +
  geom_density(data = df, aes(x = cors_fisher_transform), color = "red") +
  labs(title = "Distribution of orig correlations and fisher transformed correlations",
       x = "",
       y = "") +
  theme_minimal() +
  scale_y_continuous(labels = abs)

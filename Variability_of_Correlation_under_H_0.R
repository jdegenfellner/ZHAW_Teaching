# Variability of correlation under H_0: rho = 0

# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Variability_of_Correlation_under_H_0.R

# # Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pacman)
p_load(janitor,tidyverse,readxl,writexl,
       data.table,robustbase,flextable, MASS)

# H_0: no correlation----

n <- 19 # sample size
n_sim <- 1000 # number of simulations

cor_vec <- numeric(n_sim)
cor_vec_spearman <- numeric(n_sim)
for(i in 1:n_sim){
  x <- rnorm(n)
  y <- rnorm(n) # create independent "instruments" 
  cor_vec[i] <- cor(x,y)
  cor_vec_spearman[i] <- cor(x,y, method = "spearman")
}

hist(cor_vec, main = "Verteilung Zufallskorrelation unter H_0")
quantile(cor_vec, probs = c(0.025, 0.975))

hist(cor_vec_spearman, main = "Verteilung Zufallskorrelation (Spearman) unter H_0")
quantile(cor_vec_spearman, probs = c(0.025, 0.975))

plot(x,y)
cor(x,y)
sum(abs(cor_vec)>0.1)/n_sim
sum(abs(cor_vec)>0.2)/n_sim
sum(abs(cor_vec)>0.3)/n_sim
sum(abs(cor_vec)>0.4)/n_sim



# H_0: rho = 0.5----------
generate_correlated_samples <- function(n, rho, mu = c(0, 0), 
                                        sigma = c(1, 1)) {
  mean_vector <- mu
  covariance_matrix <- matrix(c(sigma[1]^2, rho * sigma[1] * sigma[2], 
                                rho * sigma[1] * sigma[2], sigma[2]^2), 
                              nrow = 2)
  samples <- mvrnorm(n, mu = mean_vector, Sigma = covariance_matrix)
  return(samples)
}

set.seed(123)  # For reproducibility
n <- 19       # Sample size
rho <- 0.5     # Desired correlation
samples <- generate_correlated_samples(n, rho)

head(samples)
df <- as.data.frame(samples)
df %>% ggplot(aes(x=V1,y=V2)) + 
  geom_point() + 
  xlab("x") + ylab("y")


# p-Value under H_0: rho = 0.5
n_sim <- 1000
rho_vec <- numeric(n_sim)
for(i in 1:n_sim){
  res <- generate_correlated_samples(19, rho = 0.5)
  rho_vec[i] <- cor(res[,1], res[,2], method = "spearman")
}
hist(rho_vec)
sum(rho_vec<=0.12)/n_sim

# p-Value under H_0: rho = 0.4
n_sim <- 1000
rho_vec <- numeric(n_sim)
for(i in 1:n_sim){
  res <- generate_correlated_samples(19, rho = 0.4)
  rho_vec[i] <- cor(res[,1], res[,2], method = "spearman")
}
hist(rho_vec)
sum(rho_vec<=0.12)/n_sim

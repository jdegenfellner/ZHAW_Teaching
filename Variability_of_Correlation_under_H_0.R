# Variability of correlation under H_0: rho = 0

# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Variability_of_Correlation_under_H_0.R

# # Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pacman)
p_load(janitor,tidyverse,readxl,writexl,
       data.table,robustbase,flextable)


n <- 35 # sample size
n_sim <- 1000 # number of simulations

cor_vec <- numeric(n_sim)
for(i in 1:n_sim){
  x <- rnorm(n)
  y <- rnorm(n) # create independent "instruments" 
  cor_vec[i] <- cor(x,y)
}
hist(cor_vec, main = "Verteilung Zufallskorrelation unter H_0")
quantile(cor_vec, probs = c(0.025, 0.975))

plot(x,y)
cor(x,y)
sum(abs(cor_vec)>0.1)/n_sim
sum(abs(cor_vec)>0.2)/n_sim
sum(abs(cor_vec)>0.3)/n_sim
sum(abs(cor_vec)>0.4)/n_sim

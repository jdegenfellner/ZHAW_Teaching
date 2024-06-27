# Prof. Andrew Gelman said something like, 

# "standard practice you get an estimate you do a 
# study and if the estimates more than two standard errors away from zero you
# report it and publish it if it's less than two standard errors away from
# zero you shake the data until you get something two standard errors away
# from zero and you publish that if you do that consistently you will
# overestimate your treatment effects because after all you can only report
# things that are at least two standard errors away from zero you'd like so
# if the true value of the treatment is effect is less than that 
# it's it's biased you know like it's biased no matter what but like it's if
# the true value is less than two standard errors it's it's like deterministically bias "

# https://www.youtube.com/watch?v=xgUBdi2wcDI&t=5222s&ab_channel=HarryCrane

# Let's try to verify some of that:

library(pacman)
p_load(tidyverse)

n <- 100
n_sim <- 1000
beta_x1 <- 0.5
beta_x2 <- 0.2
coef_x1 <- rep(NA, n_sim)
coef_x2 <- rep(NA, n_sim)
p_val_x1 <- rep(NA, n_sim)
p_val_x2 <- rep(NA, n_sim)

for(i in 1:n_sim){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  y <- beta_x1*x1 + beta_x2*x2 + rnorm(n)
  
  df <- data.frame(x1 = x1,
                   x2 = x2,
                   y = y)
  
  mod <- lm(y ~ x1 + x2, data = df)
  
  coef_x1[i] <- coef(mod)[2]
  coef_x2[i] <- coef(mod)[3]
  
  summary_mod <- summary(mod)
  
  p_val_x1[i] <- summary_mod$coefficients["x1", "Pr(>|t|)"]
  p_val_x2[i] <- summary_mod$coefficients["x2", "Pr(>|t|)"]
}

df_res <- data.frame(p_val_x1 = p_val_x1,
                     p_val_x2 = p_val_x2,
                     coef_x1 = coef_x1,
                     coef_x2 = coef_x2)

# alpha = 0.05------
df_res %>% filter(p_val_x1 < 0.05) %>%
  ggplot(aes(x = coef_x1)) + 
  geom_histogram() + 
  geom_vline(xintercept = beta_x1) + 
  geom_vline(xintercept = mean(df_res[p_val_x1 < 0.05,]$coef_x1), color="red")
# # looks fine!
  
df_res %>% filter(p_val_x2 < 0.05) %>%
  ggplot(aes(x = coef_x2)) + 
  geom_histogram() + 
  geom_vline(xintercept = beta_x2) +
  geom_vline(xintercept = mean(df_res[p_val_x2 < 0.05,]$coef_x2), color="red")
# rather skewed.


# The smaller the p-value, the larger the t-statistic for the coefficient,
# and this is the case when the estimate is rather large compared to H_0: mue=0.

# We can suspect, that the bias is larger with even smaller p_values, let's check:

# alpha = 0.005----
df_res %>% filter(p_val_x2 < 0.005) %>%
  ggplot(aes(x = coef_x2)) + 
  geom_histogram() + 
  geom_vline(xintercept = beta_x2)+ # rather skewed.
  geom_vline(xintercept = mean(df_res[p_val_x2 < 0.005,]$coef_x2), color="red")

# alpha = 0.0005
df_res %>% filter(p_val_x2 < 0.0005) %>%
  ggplot(aes(x = coef_x2)) + 
  geom_histogram() + 
  geom_vline(xintercept = beta_x2) +# rather skewed.
  geom_vline(xintercept = mean(df_res[p_val_x2 < 0.0005,]$coef_x2), color="red")
# slightly larger even


# Is this different/better when using Bayes?-------
# TODO----

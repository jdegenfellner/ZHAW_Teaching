# Gelman, you overestimate effects, when you have a significant effect

# in progress #

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

df_res %>% filter(p_val_x1 < 0.05) %>%
  ggplot(aes(x = coef_x1)) + 
  geom_histogram() + 
  geom_vline(xintercept = beta_x1) # looks fine!

df_res %>% filter(p_val_x2 < 0.05) %>%
  ggplot(aes(x = coef_x2)) + 
  geom_histogram() + 
  geom_vline(xintercept = beta_x2) # rather skewed.

df_res %>% 
  filter(p_val_x2 < 0.05) %>%
  dplyr::summarize(mean_coef_x2 = mean(coef_x2, na.rm = TRUE))
# overestimated systematically


mean(coef_x1) # very good
mean(coef_x2) # very good

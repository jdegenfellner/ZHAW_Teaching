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
  
  coef_x1[i] <- coef(mod)[1]
  coef_x2[i] <- coef(mod)[2]
  
  summary_mod <- summary(mod)
  
  p_value_x1[i] <- summary_mod$coefficients["x1", "Pr(>|t|)"]
  p_value_x2[i] <- summary_mod$coefficients["x2", "Pr(>|t|)"]
}



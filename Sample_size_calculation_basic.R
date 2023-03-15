# Sample size calculations # 
# Some basic examples # 

# Some helpful links/books:
# https://med.und.edu/research/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf and maybe
# https://med.und.edu/research/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module_glmm2.pdf

# Chapter 10 of: http://www.cs.uni.edu/~jacobson/4772/week11/R_in_Action.pdf
# Chapter 4.4 of: https://link.springer.com/book/10.1007/978-3-319-19425-7





# 1) Mini-Example from QM1 Script, p. 176---------------------------------------
power.t.test(delta = 0.5, sd = 1, power = 0.8, type = "two.sample", alternative = "one.sided")
# -> n = 50 required
n_required <- 50

# __Check via simulation:----
n <- 1000
p_vals <- rep(NA, n)
for(i in 1:n){
  x1 <- rnorm(n_required, mean = 2.5, sd = 1) # hence, effect size = (2.5 - 2.0)/1
  x2 <- rnorm(n_required, mean = 2.0, sd = 1)
  test <- t.test(x1, x2, alternative = "greater")
  p_vals[i] <- test$p.value
}
sum(p_vals < 0.05)/n # = power --> worked!!




# 2) Real-life-Example: two-armed RCT: -----------------------------------------

# __Determine alpha and power----
alpha <- 0.05
power <- 0.8

# __Determine sample size----
# ES rate - Jennings 2015
(effect_size <- (14.1-17.5)/12.5) # approx equal variances
# C-reactive protein - Niedermann 2013
(effect_size <- (4.95-6.27)/7.8) # approx equal variances

# ES rate - Jennings 2015
# https://thenewstatistics.com/itns/2021/06/17/which-standardised-effect-size-measure-is-best-when-variances-are-unequal/
(effect_size <- effect_size_d_star <- (31.1-17.5)/sqrt(16.4^2/2 + 10.5^2/2)) # try Cohen's d_star
# C-reactive proteine - Jennings 2015
(effect_size <- effect_size_d_star <- (4.95-7.14)/sqrt(4.86^2/2 + 8.21^2/2)) # try Cohen's d_star

if(effect_size > 0){ 
  alternative <- "greater"
} else if (effect_size < 0) {
  alternative <- "less"
}
# __Determine power----
pwr.t.test(d = effect_size, 
           n = NULL, 
           sig.level = alpha, 
           power = power, 
           type = "two.sample", 
           alternative = alternative) # One-side should work here since we are interested in a decrease in inflammation

# __Try range of effect sizes to illustrate influence:----
effect_sizes <- seq(0.05, 0.9, by = 0.05)
sample_sizes <- rep(NA, length(effect_sizes))

for(i in 1:length(effect_sizes)){
  test <- pwr.t.test(d = effect_sizes[i], 
                     n = NULL, 
                     sig.level = alpha, 
                     power = power, 
                     type = "two.sample", 
                     alternative = "greater") 
  sample_sizes[i] <- test$n
}

df <- data.frame(effect_sizes = effect_sizes, sample_size = sample_sizes)
df %>% filter(effect_sizes >= 0.2) %>%
  ggplot(aes(x = effect_sizes, y = sample_size)) + 
  geom_line() + 
  geom_point(x = 0.20, y = 309.80647, color = "blue", size = 5) + # highlighting typical effect sizes
  annotate("text", x = 0.25, y = 309.80647, label = "(0.20, 310)", color = "blue") +
  geom_point(x = 0.50, y = 50.15080, color = "blue", size = 5) +
  annotate("text", x = 0.55, y = 50.15080, label = "(0.50, 50)", color = "blue") +
  geom_point(x = 0.80, y = 20, color = "blue", size = 5) +
  annotate("text", x = 0.8, y = 40, label = "(0.80, 20)", color = "blue") +
  ggtitle("Effect sizes and sample sizes (per group) for power = 0.8 and alpha = 0.05") + 
  xlab("Effect size") + ylab("Sample size per group") + 
  theme_dark() + 
  theme(plot.title = element_text(hjust = 0.5))


# 3) Sample size calculation for a multiple linear regression model-------------
library(pwr)

# Let's pretend we know the true model
n <- 25
x1 <- rnorm(n = n, mean = 4, sd = 2)
x2 <- rnorm(n = n, mean = 7, sd = 2.3)
y <- 2*x1 - 2.7*x2 + rnorm(n = n, mean = 0, sd = 10) # Create outcome y and add relatively strong noise
df <- data.frame(x1 = x1, x2 = x2, y = y)
mod <- lm(y ~ x1 + x2, data = df)
summary(mod)
confint(mod)
#plot(mod$residuals)

mod_x1 <- lm(y ~ x1, data = df)
summary(mod_x1)
mod_x2 <- lm(y ~ x2, data = df)
summary(mod_x2)

# Cohens f2:
# see e.g. https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3328081/

# __a) Cohen's f^2 for global (!) effect size----
Rsquared <- summary(mod)$r.squared
(f_2 <- Rsquared/(1 - Rsquared)) # Ratio of explained vs. unexplained variance
pwr.f2.test(u = 2, v = n - 2 - 1, f2 = f_2, sig.level = 0.05, power = NULL) # degrees of freedom see also summary() output!
# This would mean that in 63% the null hypothesis that x1 and x2 explain nothing of y
# is correctly rejected.

# ___Check this via simulation.----
library(broom)
nn <- 1000
p_vals <- rep(NA, n)
for(i in 1:nn){
  n <- 25
  x1 <- rnorm(n = n, mean = 4, sd = 2)
  x2 <- rnorm(n = n, mean = 7, sd = 2.3)
  y <- 2*x1 - 2.7*x2 + rnorm(n = n, mean = 0, sd = 10) # Create outcome y and add relatively strong noise
  df <- data.frame(x1 = x1, x2 = x2, y = y)
  mod <- lm(y ~ x1 + x2, data = df)
  p_vals[i] <- glance(mod)$p.value
}
sum(p_vals<0.05)/nn # = power # magnitude is correct :)

# __b) Cohen's f^2 for: x2 explains a percentage more than x1 alone:----
(f_2_x2 <- (summary(mod)$r.squared - summary(mod_x1)$r.squared)/(1 - summary(mod)$r.squared))
pwr.f2.test(u = 1, v = n - 1 - 1, f2 = f_2_x2, sig.level = 0.05, power = NULL)
# power = 0.7553003

# ___Check via simulation----
nn <- 1000
p_vals <- rep(NA, n)
for(i in 1:nn){
  n <- 25
  x1 <- rnorm(n = n, mean = 4, sd = 2)
  x2 <- rnorm(n = n, mean = 7, sd = 2.3)
  y <- 2*x1 - 2.7*x2 + rnorm(n = n, mean = 0, sd = 10) # Create outcome y and add relatively strong noise
  df <- data.frame(x1 = x1, x2 = x2, y = y)
  mod <- lm(y ~ x1 + x2, data = df)
  #summary(mod)
  #plot(mod$residuals)
  
  mod_x1 <- lm(y ~ x1, data = df)
  #summary(mod_x1)
  mod_x2 <- lm(y ~ x2, data = df)
  #summary(mod_x2)
  
  test <- anova(mod_x2, mod)
  p_vals[i] <- test$`Pr(>F)`[2]
}
sum(p_vals<0.05)/nn # = power

# Probabilites of simultaneous birthdays

# Set working directory to source file location:
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

if (!require(pacman)) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse)

n <- 10:50
probs_exact <- round(1-choose(365,n)*factorial(n)/365^n,digits=4) # exact probabilities (Exercises Problem 9)
probs_exact
probs_approx <- 1-exp(-(n^2/(2*365))) # Approximation heuristically by my colleague Thomas.

df <- data.frame(n = n, ExactProbs = probs_exact, ApproxProbs = probs_approx)

df %>% ggplot(aes(x=n)) + 
  geom_line(aes(x=n, y=probs_approx, colour = "approx")) + 
  geom_line(aes(x=n, y=probs_exact, colour = "exact")) + 
  theme(legend.title = element_blank())

options(digits = 6)
max(probs_approx-probs_exact) # maximum error
#0.0123693

# Determine parameter(s) using (non-linear) regression
model <- nls(ExactProbs ~ 1 - exp(-a * n^2), data = df, start = list(a = 0.01))
summary(model)

df$PredictedProbs <- predict(model, newdata = df)
ggplot(df, aes(x = n)) +
  geom_point(aes(y = ExactProbs), colour = "blue") + 
  geom_line(aes(y = PredictedProbs), colour = "red") +
  labs(title = "Nonlinear Regression Model Fitting",
       x = "n", y = "Probabilities") +
  theme_minimal()

max(df$PredictedProbs - df$ExactProbs)
#0.00989698; a little improvement 

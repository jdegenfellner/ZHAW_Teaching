library(pacman)

# Multicollinearity
# Multicollinearity should not be confused with a raw strong correlation between predictors. 
# What matters is the association between one or more predictor variables, conditional on the 
# other variables in the model. In a nutshell, multicollinearity means that once you know the
# effect of one predictor, the value of knowing the other predictor is rather low. Thus, one of
# the predictors doesn't help much in terms of better understanding the model or predicting the outcome. As a consequence, if multicollinearity is a problem, the model seems to suggest that the predictors in question don't seems to be reliably associated with the outcome (low estimates, high standard errors), although these predictors actually are strongly associated with the outcome, i.e. indeed might have strong effect (McElreath 2020, chapter 6.1).
# 
# Multicollinearity might arise when a third, unobserved variable has a causal effect on each
# of the two predictors that are associated with the outcome. In such cases, the actual
# relationship that matters would be the association between the unobserved variable and the outcome.
# 
# Remember: "Pairwise correlations are not the problem. It is the conditional associations - 
# not correlations - that matter." (McElreath 2020, p. 169)

p_load(
  MASS,
  car,
  performance,
  tidyverse
)

set.seed(123)

n <- 500

Sigma <- matrix(
  c(
    1.0, 0.8, 0.8, 0.8,
    0.8, 1.0, 0.8, 0.8,
    0.8, 0.8, 1.0, 0.8,
    0.8, 0.8, 0.8, 1.0
  ),
  nrow = 4,
  byrow = TRUE
)

X <- MASS::mvrnorm(
  n = n,
  mu = c(0, 0, 0, 0),
  Sigma = Sigma
)

df <- tibble(
  x1 = X[,1],
  x2 = X[,2],
  x3 = X[,3],
  x4 = X[,4]
)

eta <-
  -1 +
  1.5 * df$x1 -
  1.0 * df$x2 +
  0.8 * df$x3 +
  0.5 * df$x4

p <- plogis(eta)

df$y <- rbinom(
  n,
  size = 1,
  prob = p
)

mod <- glm(
  y ~ x1 + x2 + x3 + x4,
  family = binomial,
  data = df
)

summary(mod)

exp(coef(mod))

cor(
  df %>%
    dplyr::select(x1:x4)
)

car::vif(mod)

performance::check_collinearity(mod)

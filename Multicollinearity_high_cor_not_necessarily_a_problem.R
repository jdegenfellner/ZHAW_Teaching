library(pacman)

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

df$y <-
  5 +
  1.5 * df$x1 -
  1.0 * df$x2 +
  0.8 * df$x3 +
  0.5 * df$x4 +
  rnorm(n, 0, 1)

mod <- lm(
  y ~ x1 + x2 + x3 + x4,
  data = df
)

summary(mod)

cor(
  df %>% dplyr::select(x1:x4)
)

car::vif(mod)

performance::check_collinearity(mod)
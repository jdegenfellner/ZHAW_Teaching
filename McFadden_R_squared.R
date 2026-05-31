library(pacman)
p_load(
  tidyverse,
  performance,
  pscl
)

# ln(L) and ln(L0) are both negative because the likelihood
# is a product of Bernoulli probabilities, each of which lies
# between 0 and 1. Consequently, the likelihood itself is also
# between 0 and 1, and its logarithm is therefore negative.
#
# See:
# https://web.stanford.edu/class/archive/cs/cs109/cs109.1178/
# lectureHandouts/220-logistic-regression.pdf
#
# As model fit improves, ln(L) becomes less negative and moves
# closer to zero. Therefore, the ratio
#
#   ln(L) / ln(L0)
#
# approaches 1 because the numerator and denominator become
# more similar in magnitude. McFadden's R² is defined as
#
#   R² = 1 - ln(L) / ln(L0)
#
# and therefore increases as the fitted model improves relative
# to the null model. McFadden's R² equals 0 when the fitted
# model is no better than the null model and approaches 1 as
# model fit improves substantially.

set.seed(123)

simulate_data <- function(n = 1000, beta0 = 0, beta1 = 0) {
  x <- stats::rnorm(n)
  p <- stats::plogis(beta0 + beta1 * x)
  y <- stats::rbinom(n, size = 1, prob = p)
  
  base::data.frame(
    x = x,
    y = y
  )
}

mcfadden_r2 <- function(model_full, model_null) {
  ll_full <- as.numeric(stats::logLik(model_full))
  ll_null <- as.numeric(stats::logLik(model_null))
  
  1 - ll_full / ll_null
  # see start of file...
}



run_simulation <- function(beta1_values, n = 1000, beta0 = 0) {
  
  out <- vector("list", length(beta1_values))
  
  for (i in seq_along(beta1_values)) {
    
    b1 <- beta1_values[i]
    
    sim_dat <- simulate_data(
      n = n,
      beta0 = beta0,
      beta1 = b1
    )
    
    model_full <- stats::glm(
      y ~ x,
      data = sim_dat,
      family = stats::binomial()
    )
    
    model_null <- stats::glm(
      y ~ 1,
      data = sim_dat,
      family = stats::binomial()
    )
    
    out[[i]] <- base::data.frame(
      beta1 = b1,
      mcfadden_r2 = mcfadden_r2(model_full, model_null)
    )
  }
  
  dplyr::bind_rows(out)
}

beta_seq <- seq(0, 30, by = 0.25)

res <- run_simulation(
  beta1_values = beta_seq,
  n = 1000,
  beta0 = 0
)

p <- ggplot(res, aes(x = beta1, y = mcfadden_r2)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  labs(
    title = "McFadden R² increases with stronger classification",
    x = expression(beta[1]),
    y = "McFadden R²"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5)
  )

p

plot_logistic_curve <- function(beta0 = 0, beta1 = 1, n = 1000) {
  
  df <- simulate_data(n = n, beta0 = beta0, beta1 = beta1)
  
  x_grid <- seq(min(df$x), max(df$x), length.out = 200)
  p_grid <- plogis(beta0 + beta1 * x_grid)
  
  df_curve <- data.frame(
    x = x_grid,
    p = p_grid
  )
  
  ggplot(df, aes(x = x, y = y)) +
    geom_jitter(height = 0.05, width = 0, alpha = 0.3) +
    geom_line(data = df_curve, aes(x = x, y = p), linewidth = 1.5) +
    labs(
      title = paste0("Logistic curve (beta0 = ", beta0, ", beta1 = ", beta1, ")"),
      x = "x",
      y = "P(y = 1 | x)"
    ) +
    theme_minimal(base_size = 15) +
    theme(plot.title = element_text(hjust = 0.5))
}

plot_logistic_curve(beta0 = 0, beta1 = 30)

# check mcfadden with different packages:----------

sim_dat <- simulate_data(
  n = 1000,
  beta0 = 0,
  beta1 = 3
)

model_full <- glm(
  y ~ x,
  data = sim_dat,
  family = binomial()
)

model_null <- glm(
  y ~ 1,
  data = sim_dat,
  family = binomial()
)

manual_mcfadden <- mcfadden_r2(
  model_full,
  model_null
)

performance_mcfadden <- performance::r2_mcfadden(
  model_full
)

pscl_mcfadden <- pscl::pR2(
  model_full
)["McFadden"]

manual_mcfadden
performance_mcfadden
pscl_mcfadden
# all identical.
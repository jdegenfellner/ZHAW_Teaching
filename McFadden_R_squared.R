library(tidyverse)

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

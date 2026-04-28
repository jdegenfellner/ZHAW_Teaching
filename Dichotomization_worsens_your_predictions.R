# Dichotomization_worsens_your_predictions.R

library(tidyverse)

#set.seed(123)

n <- 300

df <- tibble(
  x = rnorm(n, mean = 10, sd = 3)
) %>%
  mutate(
    y = 2 + 0.8 * x + rnorm(n, 0, 2)
  )

m_cont <- lm(y ~ x, data = df)

base_row <- tibble(
  model = "continuous x",
  cutoff = NA_real_,
  rse = summary(m_cont)$sigma,
  r2 = summary(m_cont)$r.squared
)

cutoffs <- quantile(df$x, probs = seq(0.05, 0.95, length.out = 10), 
                    names = FALSE)

dicho_rows <- map_dfr(cutoffs, function(cut) {
  df2 <- df %>% mutate(x_bin = as.integer(x > cut))
  df2$x_bin <- as.factor(df2$x_bin) 
  m_bin <- lm(y ~ x_bin, data = df2)
  tibble(
    model = "dichotomized x",
    cutoff = cut,
    rse = summary(m_bin)$sigma,
    r2 = summary(m_bin)$r.squared
  )
})

results <- bind_rows(base_row, dicho_rows) %>%
  mutate(
    rse_change = rse - base_row$rse,
    rse_ratio = rse / base_row$rse
  ) %>%
  arrange(is.na(cutoff), cutoff)

results


ggplot(results %>% filter(!is.na(cutoff)), aes(x = cutoff, y = rse)) +
  geom_point() +
  geom_hline(yintercept = results$rse[results$model == "continuous x"], linetype = 2) +
  labs(
    title = "Residual standard error: continuous vs dichotomized x",
    x = "Cutoff (quantiles of x)",
    y = "Residual standard error"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))

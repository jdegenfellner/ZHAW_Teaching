# Coin toss with Bayes

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pacman)
p_load(mlbench,
       rstanarm,
       bayestestR,
       bayesplot,
       insight,
       broom,
       tidyverse,
       flextable,
       tictoc,
       performance,
       brms,
       posterior)

# We toss a toin 20 times
data <- data.frame(y = c(rep(1, 6), rep(0, 14))) # result of coin toss
data

# Maximum Likelihood (ML) estimate----
theta <- seq(from = 0.01, to = 0.99, by = 0.01)
ind_theta_ML <- which.max(theta^6*(1-theta)^(20-6))
plot(theta, log(theta^6*(1-theta)^(20-6)))
theta[ind_theta_ML] # 0.3


# Bayes estimation----
# Define different priors for the intercept in the logit-regression model

#priors <- list(
#  prior(beta(1, 1), class = "Intercept"),  # Uniform prior
#  prior(beta(2, 2), class = "Intercept"),  # Beta(2,2) prior
#  prior(beta(0.5, 0.5), class = "Intercept")  # Beta(0.5,0.5) prior
#)
# ----> this is not working since we are operating on the logit-scale in the regression and we would land outside the range of [0,1] when backtransforming to the probability-scale

priors <- list(
  prior(normal(0, 1), class = "Intercept"),  # Normal prior
  prior(student_t(3, 0, 2.5), class = "Intercept"),  # Student-t prior
  prior(cauchy(0, 2), class = "Intercept")  # Cauchy prior
)

tic()
results <- list()
for (i in 1:length(priors)) {
  fit <- brm(y ~ 1, family = bernoulli(), 
             data = data, 
             prior = priors[[i]], 
             iter = 4000, chains = 4, seed = 123)
  results[[i]] <- fit
}
toc() # 65.25 sec elapsed, no errors

# logistic function
inv_logit <- function(x) {
  exp(x) / (1 + exp(x))
}

# Plot posterior
posterior_plots <- lapply(1:length(results), function(i) {
  draws <- as_draws_df(results[[i]], variable = "b_Intercept")
  draws$theta <- inv_logit(draws$b_Intercept)  # Transformiere den Intercept in \(\theta\)
  ggplot(draws, aes(x = theta)) +
    geom_density() +
    labs(title = paste("Prior:", as.character(priors[[i]]))) + 
    theme(plot.title = element_text(hjust = 0.5))
})

# Probability, that the intercept in model 1 is negative given a priori and data?----
all_values <- as_draws_df(results[[1]], variable = "b_Intercept")$b_Intercept
sum(all_values<0)/length(all_values) # logit scale, intercept!
sum(inv_logit(all_values)<0.4)/length(inv_logit(all_values)) # on the theta scale

for (plot in posterior_plots) {
  print(plot)
}

# _Estimates via Bayes----
theta_estimates <- sapply(results, function(model) {
  intercept <- fixef(model)["Intercept", "Estimate"]
  inv_logit(intercept)
})

theta_estimates

# Draw priors for theta:----
# Note, that the above priors where in relation to the intercept in the logit-regression, 
# not in relation to theta itself
n_samples <- 10000
samples_normal <- rnorm(n_samples, mean = 0, sd = 1)
samples_student_t <- rt(n_samples, df = 3) * 2.5
samples_cauchy <- rcauchy(n_samples, location = 0, scale = 2)

# Logit-transformation
theta_normal <- inv_logit(samples_normal)
theta_student_t <- inv_logit(samples_student_t)
theta_cauchy <- inv_logit(samples_cauchy)

data <- data.frame(
  theta = c(theta_normal, theta_student_t, theta_cauchy),
  distribution = factor(rep(c("Normal(0, 1)", "Student-t(3, 0, 2.5)", "Cauchy(0, 2)"), each = n_samples))
)

ggplot(data, aes(x = theta, fill = distribution)) +
  geom_density(alpha = 0.5) +
  labs(title = "Logit-transformed Prior Distributions",
       x = expression(theta),
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))



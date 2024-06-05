# GLM - simple example of maximum likelihood (ML) estimation using coin flip

theta <- seq(from=0.01, to = 0.99, by = 0.001)
theta

likelihood_f <- theta^6*(1-theta)^14 # 9 heads, 8 tails

plot(theta, likelihood_f)

which.max(likelihood_f)

theta[which.max(likelihood_f)] # 0.3

data.frame(theta = theta, log_likelihood = log(likelihood_f)) %>%
ggplot(aes(x = theta, y = log_likelihood)) +
  geom_line() +
  geom_vline(xintercept = 0.3, color = "red", linetype = "dashed") +
  labs(x = "theta", y = "log(likelihood_f)", title = "Log-Likelihood") +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) + 
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12))

log_likelihood <- log(likelihood_f)

which.max(log_likelihood)

#---------
# Plot logit function

p <- seq(from=0.000001, to = 0.9999999099, by = 0.001)
p/(1-p) # odds
plot(p, log(p/(1-p)))

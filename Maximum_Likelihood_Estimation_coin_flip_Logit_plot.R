# GLM - simple example of maximum likelihood (ML) estimation using coin flip

theta <- seq(from=0.01, to = 0.99, by = 0.001)
theta

likelihood_f <- theta^9*(1-theta)^8 # 9 heads, 8 tails

plot(theta, likelihood_f)

which.max(likelihood_f)

theta[520]

plot(theta, log(likelihood_f))

log_likelihood <- log(likelihood_f)

which.max(log_likelihood)

#---------
# Plot logit function

p <- seq(from=0.000001, to = 0.9999999099, by = 0.001)
p/(1-p) # odds
plot(p, log(p/(1-p)))

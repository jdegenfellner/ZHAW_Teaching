# Gelman, you overestimate effects, when you have a significant effect

# in progress #

n_sim <- 100
x1 <- rnorm(n_sim)
x2 <- rnorm(n_sim)
y <- 0.5*x1 + 0.3*x2 + rnorm(n_sim)

df <- data.frame(x1 = x1
                 x2 = x2,
                 y = y)

mod <- lm(y ~ x1 + x2, data = df)
summary(mod)

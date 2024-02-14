# (in progress)

# Significance testing and veracity of estimation
# Andrew Gelman and others remarked that if a statistical test is significant,
# one can expect the "effect" estimate to be exaggerated.

# "Indeed, if power is too low, a significant result can only be misleading: 
# Any good estimate will be nonsignificant."
# https://doi.org/10.1016/j.rmal.2023.100059

# Power could be estimated more precisely, 
# small n and large sd should give low power for now.

n <- 20
x1 <- rnorm(n, mean = 0, sd = 2)
x2 <- rnorm(n, mean = 0, sd = 2.3)
cor(x1,x2)
y <- -2*x1 + 3*x2 + rnorm(n)
df <- data.frame(x1=x1, x2=x2, y=y)

mod <- lm(y ~ x1 + x2, data = df)
summary(mod)

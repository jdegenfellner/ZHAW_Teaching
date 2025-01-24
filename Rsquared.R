# How large is R^2 if we only have noise?

library(tictoc)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

n <- 5000
r_squared <- rep(NA, n)

plot(rnorm(n), rnorm(n))

tic()
for(i in 1:n){
  x <- rnorm(n)
  y <- rnorm(n)
  df <- data.frame(x = x, y = y)
  s <- summary(lm(y ~ x, data = df)) # simple linear regression
  r_squared[i] <- s$r.squared
}
toc() # for measuring time

hist(r_squared, main = "Histrogram of R^2", xlab = "R^2")
quantile(r_squared, 0.99)

# TODO
# add more covariates
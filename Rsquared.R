# Bestimmtheitsmass, wie groß ist dieses, wenn man nur Rauschen hat?

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
  s <- summary(lm(y ~ x, data = df))
  r_squared[i] <- s$r.squared
}
toc()

hist(r_squared)

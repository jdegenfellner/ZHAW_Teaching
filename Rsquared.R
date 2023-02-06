# Bestimmtheitsmass, wie groß ist dieses, wenn man nur Rauschen hat?

n <- 1000
r_squared <- rep(NA, 1000)

for(i in 1:n){
  x <- rnorm(1000)
  y <- rnorm(1000)
  df <- data.frame(x = x, y = y)
  s <- summary(lm(y ~ x, data = df))
  r_squared[i] <- s$r.squared
}

hist(r_squared)

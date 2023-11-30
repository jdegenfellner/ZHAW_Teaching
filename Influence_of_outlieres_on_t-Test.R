# This problem was proposed to me by my friend Matthias Templ :)

# Consider the t-Test
gr1 <- rnorm(40, mean = 0, sd = 1)
gr2 <- rnorm(40, mean = 1, sd = 1)
t.test(gr1, gr2)

# Introduce an outlier
gr2[3] <- gr2[3] + 100
mean(gr2)
sd(gr2)

# What happens to the p-Value and why?
t.test(gr1, gr2)$p.value

# Hint: visualize the outlier influence on the difference in means and
# on the standard deviation of gr2



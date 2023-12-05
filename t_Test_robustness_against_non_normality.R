# t-Test robustness against non-normality in DATA:

library(tidyverse)

# plot-function:
plot_random_variables <- function(x1, x2) {
  library(ggplot2)
  
  df_x1 = data.frame(x = x1, variable = "x1")
  df_x2 = data.frame(x = x2, variable = "x2")
  df = rbind(df_x1, df_x2)
  
  ggplot(df, aes(x = x, fill = variable)) +
    geom_density(alpha = 0.7) +
    labs(x = "X", y = "Density", title = "Comparison of Two Random Variables") +
    scale_fill_manual(values = c("red", "blue"))
}

n <- 20

# Consider
x1 <- rnorm(n, mean = 0, sd = 1)
x2 <- rnorm(n, mean = 1, sd = 1)
plot_random_variables(x1, x2)
t.test(x1, x2, alternative = "two.sided") # correct decision

# 1) Change x1:----
x1 <- rt(n, df = 4, ncp = 4)
x2 <- rnorm(n, mean = mean(x1)+1, sd = 1)

plot_random_variables(x1, x2)
t.test(x1, x2, alternative = "two.sided") # large p.value

# TODO:
# 2) Plot the p.values for a range of n and ncp and interpret the result
# 3) Change x2 to skewed distribution with same mean:


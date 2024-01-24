# Some code for simple linear regression
# From https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Simple_Linear_Regression.R
# 24.1.24
# QM2 
# See also Script QM2 (Meichtry, S.2-13)
# See also https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Simple_linear_regression_shiny.R

# Example data for simple linear regression----
# https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regressionSimple.csv
# https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regression.csv

library(pacman)
p_load(plotly, tidyverse)

# ad page 5 Script ----
# Let's look at formula (1.2.6) and use data from above
df <- read.csv("https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regressionSimple.csv")
str(df)
mod <- lm(y ~ x, data = df) # least squares method.
summary(mod)

sse <- function(alpha, beta, data) {
  predicted <- alpha + beta * data$x
  sum((data$y - predicted)^2)
}

alpha_values <- seq(-5, 5, length.out = 100)
beta_values <- seq(-5, 5, length.out = 100)
sse_values <- outer(alpha_values, beta_values, Vectorize(function(a, b) sse(a, b, df)))
fig <- plot_ly(x = ~alpha_values, y = ~beta_values, z = ~sse_values, type = "surface")
fig

# Highlight min
min_sse_index <- which(sse_values == min(sse_values), arr.ind = TRUE)
min_alpha <- alpha_values[min_sse_index[1]]
min_beta <- beta_values[min_sse_index[2]]
min_sse <- min(sse_values)

fig <- fig %>% add_markers(x = ~min_alpha, y = ~min_beta, z = ~min_sse, 
                           marker = list(color = 'red', size = 10))
fig


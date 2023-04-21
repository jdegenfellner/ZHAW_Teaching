# Neuronal networks are extensions of linear regression models

#[this script was partially created using GPT4]

library(nnet)
library(tidyverse)
library(ggalt)
library(neuralnet)

set.seed(42)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 2 + 3 * x1 + 4 * x2 + rnorm(100, sd = 0.5)
data <- data.frame(x1 = x1, x2 = x2, y = y)

formula <- y ~ x1 + x2
mod <- lm(formula = formula, data = data)
summary(mod)

nn <- neuralnet(formula, data, hidden = 0, linear.output = TRUE)

plot(nn)

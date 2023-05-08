# Neuronal networks are extensions of linear regression models

#v[this script was partially created using GPT-4]

library(nnet)
library(tidyverse)
library(ggalt)
library(neuralnet)

set.seed(42)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 2 + 3 * x1 + 4 * x2 + rnorm(100, sd = 0.5)
data <- data.frame(x1 = x1, x2 = x2, y = y)

mod <- lm(y ~ x1 + x2, data = data)
summary(mod)

nn <- neuralnet(y ~ x1 + x2, data = data, 
                hidden = 0, 
                linear.output = TRUE, # If act.fct should not be applied to the output neurons set linear output to TRUE, otherwise to FALSE.
                act.fct = "logistic")

plot(nn)


# Exercises:

# a) What happens if linear.output = FALSE?
# b) Experiment with different activation functions, e.g. ReLu.
# c) Try to create a very complicated function y = f(x1,...,xn) and try
#.   to approximate it using neuralnet. 
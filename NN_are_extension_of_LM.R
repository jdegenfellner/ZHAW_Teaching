# Neuronal networks are extensions of linear regression models

#[this script was partially created using GPT-4]
# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/NN_are_extension_of_LM.R

library(nnet)
library(pacman)
p_load(tidyverse, ggalt, neuralnet)

set.seed(42)
x1 <- rnorm(100)
x2 <- rnorm(100)
y <- 2 + 3 * x1 + 4 * x2 + rnorm(100, sd = 0.5)
data <- data.frame(x1 = x1, x2 = x2, y = y)

mod <- lm(y ~ x1 + x2, data = data)
summary(mod)
plot(residuals(mod)) # looks good

# Try different act.fcts:
relu <- function(x) sapply(x, function(z) max(0,z)) # error...
# see: https://stackoverflow.com/questions/34532878/package-neuralnet-in-r-rectified-linear-unit-relu-activation-function

sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

nn <- neuralnet(y ~ x1 + x2, data = data, 
                hidden = 0, # number of hidden layers
                linear.output = TRUE, # If act.fct should NOT be applied to the output neurons set linear output to TRUE, otherwise to FALSE.
                act.fct = sigmoid) # not active right now.

plot(nn)
data$y - predict(nn, newdata = data) # residuals
summary(nn)

# Exercises:

# a) What happens if linear.output = FALSE?
# b) Experiment with different activation functions (?neuralnet), e.g. ReLu.
# c) Try adding more layers and compare the results.
# d)* Try to create a very complicated function y = f(x1,...,xn) and try
#.   to approximate it using neuralnet. 
# e) What happens if we try to model a non-linear relationship, 
#    e.g. y = x_1^2 + x_2^3 with the same model (without the activation function?)

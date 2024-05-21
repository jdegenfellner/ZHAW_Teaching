# Neuronal networks are extensions of linear regression models

#[this script was partially created using GPT-4]
# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/NN_are_extension_of_LM.R

library(pacman)
p_load(tidyverse, ggalt, neuralnet, nnet)

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
# f) Try to create a simple example of logistic regression and find the corresponding 
#    NN to get the same parameter estimates!



#----##

# ad f)
# n=100 shows slight differences in predictions and weights
# n=1000 is more aligned


set.seed(42)
n <- 100 # n=1000 is more aligned
x1 <- rnorm(n)
x2 <- rnorm(n)
prob <- 1 / (1 + exp(-(-1 + 2 * x1 - 3 * x2))) # Logistic function to generate probabilities
y <- rbinom(n, 1, prob) # Generate binary outcome

data <- data.frame(x1 = x1, x2 = x2, y = y)

# Classic logistic regression model
logit_model <- glm(y ~ x1 + x2, data = data, family = binomial)
summary(logit_model)

sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

nn <- neuralnet(y ~ x1 + x2, data = data,
                hidden = 0, # No hidden layers to mimic logistic regression
                linear.output = FALSE, # We want a classification output
                act.fct = sigmoid, # Sigmoid activation function
                stepmax = 1e5, # Increase the number of steps
                threshold = 0.001) # Convergence threshold
plot(nn)

predicted_nn <- predict(nn, newdata = data[, c("x1", "x2")])

data$logit_pred <- predict(logit_model, type = "response")
data$nn_pred <- predicted_nn

logit_coefs <- coef(logit_model)
nn_weights <- nn$result.matrix

logit_coefs
nn_weights

data$residuals_logit <- data$y - data$logit_pred
data$residuals_nn <- data$y - data$nn_pred

par(mfrow = c(1, 2))
plot(data$residuals_logit, main = "Logistic Regression Residuals")
plot(data$residuals_nn, main = "Neural Network Residuals")

# Plot the predictions for visual comparison
ggplot(data, aes(x = logit_pred, y = nn_pred)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Comparison of Predictions",
       x = "Logistic Regression Predictions",
       y = "Neural Network Predictions") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5))
  

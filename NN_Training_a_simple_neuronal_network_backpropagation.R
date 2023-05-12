# Training a simple neuronal network using backpropagation

# in progress # 

# (Initial) instruction to GPT-4:
# "I want to explain backpropagation training to my students. I want to 
# present a nice R script showing step by step the training process for a 
# small, simple neural network.
# So, let's use a data set that is included in R or in one of it's popular 
# packages and do that. Maybe you can also visualize the updated weights 
# in a step by step manner as the training data comes in."


# Load necessary libraries
library(tidyverse)

# Sigmoid function
sigmoid <- function(x) {
  1 / (1 + exp(-x)) 
}

# Derivative of sigmoid function
sigmoid_derivative <- function(x) {
  x * (1 - x) # wrong...
}

# Initialize weights and biases
initialize_weights <- function(input_neurons, hidden_neurons, output_neurons) {
  W1 <- matrix(runif(input_neurons*hidden_neurons, -1, 1), input_neurons, hidden_neurons)
  b1 <- matrix(runif(hidden_neurons, -1, 1), 1, hidden_neurons)
  W2 <- matrix(runif(hidden_neurons*output_neurons, -1, 1), hidden_neurons, output_neurons)
  b2 <- matrix(runif(output_neurons, -1, 1), 1, output_neurons)
  list(W1 = W1, b1 = b1, W2 = W2, b2 = b2)
}

# Feedforward
feedforward <- function(x, weights) {
  Z1 <- x %*% weights$W1 + weights$b1
  A1 <- sigmoid(Z1)
  Z2 <- A1 %*% weights$W2 + weights$b2
  A2 <- sigmoid(Z2)
  list(A1 = A1, A2 = A2)
}

# Backpropagation
backpropagation <- function(x, y, weights, ff_result, learning_rate) {
  dZ2 <- ff_result$A2 - y
  dW2 <- t(ff_result$A1) %*% dZ2
  db2 <- colSums(dZ2)
  dZ1 <- dZ2 %*% t(weights$W2) * sigmoid_derivative(ff_result$A1)
  dW1 <- t(x) %*% dZ1
  db1 <- colSums(dZ1)
  
  weights$W1 <- weights$W1 - learning_rate * dW1
  weights$b1 <- weights$b1 - learning_rate * db1
  weights$W2 <- weights$W2 - learning_rate * dW2
  weights$b2 <- weights$b2 - learning_rate * db2
  weights
}

# Neural network training
train_nn <- function(x, y, input_neurons, hidden_neurons, output_neurons, learning_rate, epochs) {
  weights <- initialize_weights(input_neurons, hidden_neurons, output_neurons)
  for (epoch in 1:epochs) {
    ff_result <- feedforward(x, weights)
    weights <- backpropagation(x, y, weights, ff_result, learning_rate)
  }
  weights
}

# Load data
data(mtcars)
x <- as.matrix(mtcars[, c('hp', 'wt')])  # input
y <- as.matrix(mtcars['mpg'])  # output

# Normalize data
x <- scale(x)
y <- scale(y)

# Parameters
input_neurons <- 2
hidden_neurons <- 5
output_neurons <- 1
learning_rate <- 0.01
epochs <- 
  
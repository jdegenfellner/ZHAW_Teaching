# Recreating plot in Bishop 2006, p.231 - showing the capability of
# neural nets (feed forward and single hidden layer) to be function approximators.

# For a proof see Hornik 1989.
# https://www.cs.cmu.edu/~epxing/Class/10715/reading/Kornick_et_al.pdf

# easy depiction of the basic idea of the proof:
# https://www.youtube.com/watch?v=Ijqkc7OLenI&ab_channel=MichaelNielsen
# https://www.youtube.com/watch?v=FWrju8jrNZ4&ab_channel=DrAnneHsu

# Instruction to GPT-4:

# Write R code to show the capability of a multi-layer perceptron 
# (3 hidden units) to approximate four different functions. 
# a) f(x) = x^2
# b) f(x) = sin(x)
# c) f(x) = abs(x)
# d) f(x) = H(x), where H is the Heavside step function.

# N = 50 data points, shown as blue dots, have been samples uniformly in x 
# over the interval (-1,1) and the corresponding values of f(x) evaluated. 

library(pacman)
p_load(neuralnet, tidyverse, gridExtra)

N <- 50
x <- seq(-1, 1, length.out = N)

# Define the functions
f1 <- function(x) x^2
f2 <- function(x) sin(x)
f3 <- function(x) abs(x)
f4 <- function(x) ifelse(x >= 0, 1, 0)

# Evaluate the functions for the dataset
y1 <- f1(x)
y2 <- f2(x)
y3 <- f3(x)
y4 <- f4(x)

data1 <- data.frame(x, y1)
data2 <- data.frame(x, y2)
data3 <- data.frame(x, y3)
data4 <- data.frame(x, y4)

plot(x,y1)

hidden_activations <- function(nn, data) {
  # Extract weights
  w1 <- nn$weights[[1]][[1]]
  w2 <- nn$weights[[1]][[2]]
  
  # Compute the hidden layer activations
  z <- data %*% w1
  a <- tanh(z)  # Apply tanh activation
  
  return(a)
}

set.seed(3244)

# fit neuronal network
nn1 <- neuralnet(y1 ~ x, data1, hidden = 3, linear.output = TRUE, act.fct = "tanh")
nn2 <- neuralnet(y2 ~ x, data2, hidden = 3, linear.output = TRUE, act.fct = "tanh")
nn3 <- neuralnet(y3 ~ x, data3, hidden = 3, linear.output = TRUE, act.fct = "tanh")
nn4 <- neuralnet(y4 ~ x, data4, hidden = 3, linear.output = TRUE, act.fct = "tanh")

activations1 <- hidden_activations(nn1, cbind(1, x))
activations2 <- hidden_activations(nn2, cbind(1, x))
activations3 <- hidden_activations(nn3, cbind(1, x))
activations4 <- hidden_activations(nn4, cbind(1, x))

predictions1 <- neuralnet::compute(nn1, data1)$net.result
predictions2 <- neuralnet::compute(nn2, data2)$net.result
predictions3 <- neuralnet::compute(nn3, data3)$net.result
predictions4 <- neuralnet::compute(nn4, data4)$net.result


generate_plot <- function(x, y, predictions, activations, title) {
  df <- data.frame(x, y, predictions, activations)
  colnames(df) <- c("x", "y", "predictions", "hidden1", "hidden2", "hidden3")
  
  p <- ggplot(df, aes(x = x)) +
    geom_point(aes(y = y), color = "blue") +
    geom_line(aes(y = predictions), color = "red") +
    geom_line(aes(y = hidden1), color = "green", linetype = "dashed") +
    geom_line(aes(y = hidden2), color = "purple", linetype = "dashed") +
    geom_line(aes(y = hidden3), color = "orange", linetype = "dashed") +
    ylim(-1, 1) +
    ggtitle(title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}

plot1 <- generate_plot(x, y1, predictions1[, 1], activations1, "f(x) = x^2")
plot2 <- generate_plot(x, y2, predictions2[, 1], activations2, "f(x) = sin(x)")
plot3 <- generate_plot(x, y3, predictions3[, 1], activations3, "f(x) = abs(x)")
plot4 <- generate_plot(x, y4, predictions4[, 1], activations4, "f(x) = H(x)")

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)

plot(nn1)


# verify first prediction of nn1 manually (given the network weights of nn1):----
y1[1]
x[1]

# Extract the weights from the neural network
nn1$weights

# Manually compute the activations of the hidden layer for the first input
x_input <- -1  # First input

#input node1 (hidden layer):
-1.983015 + x_input*-2.142370
#output node 1 (using tanh):
tanh(-1.983015 + x_input*-2.142370) -> out_node1

# input node2:
-1.374763 + x_input*1.683648
# output node2:
tanh(-1.374763 + x_input*1.683648) -> out_node2

# input node3:
-0.8021191 + x_input*-1.0871385
tanh(-0.8021191 + x_input*-1.0871385) -> out_node3

# y1:
1.6999890 + out_node1*0.5344631 + out_node2*0.9555455 + out_node3*0.5182628
  
  

  
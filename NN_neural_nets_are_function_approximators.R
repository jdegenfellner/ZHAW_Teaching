# Recreating plot in Bishop 2006, p231 - showing the capability of
# neural nets to be function approximators.

# For a proof see Hornik 1989.

# GPT-4

# Write R code to sho the capability of a multilayer perceptron (3 hidden units) to approximate four different functions. 
# a) f(x) = x^2
# b) f(x) = sin(x)
# c) f(x) = abs(x)
# d) f(x) = H(x), where H is the Heavside step function.

# N = 50 data points, shown as blue dots, have been samples uniformly in x over the interval (-1,1) and the corresponding values of f(x) evaluated. 

#install.packages("neuralnet")
library(neuralnet)

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

# Train the neural networks
nn1 <- neuralnet(y1 ~ x, data1, hidden = 3, linear.output = TRUE)
nn2 <- neuralnet(y2 ~ x, data2, hidden = 3, linear.output = TRUE)
nn3 <- neuralnet(y3 ~ x, data3, hidden = 3, linear.output = TRUE)
nn4 <- neuralnet(y4 ~ x, data4, hidden = 3, linear.output = TRUE)

predictions1 <- compute(nn1, data1)$net.result
predictions2 <- compute(nn2, data2)$net.result
predictions3 <- compute(nn3, data3)$net.result
predictions4 <- compute(nn4, data4)$net.result

library(ggplot2)
library(gridExtra)

generate_plot <- function(x, y, predictions, title) {
  df <- data.frame(x, y, predictions)
  colnames(df) <- c("x", "y", "predictions")
  p <- ggplot(df, aes(x = x)) +
    geom_point(aes(y = y), color = "blue") +
    geom_line(aes(y = predictions), color = "red") +
    ylim(-1, 1) +
    ggtitle(title) +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5))
  return(p)
}

plot1 <- generate_plot(x, y1, predictions1[, 1], "f(x) = x^2")
plot2 <- generate_plot(x, y2, predictions2[, 1], "f(x) = sin(x)")
plot3 <- generate_plot(x, y3, predictions3[, 1], "f(x) = abs(x)")
plot4 <- generate_plot(x, y4, predictions4[, 1], "f(x) = H(x)")

grid.arrange(plot1, plot2, plot3, plot4, nrow = 2, ncol = 2)



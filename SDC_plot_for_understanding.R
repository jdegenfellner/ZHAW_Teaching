# SDC plot for better understanding

library(tidyverse)

library(ggplot2)

# Generate data
x <- seq(-3, 3, length.out = 1000)
y <- dnorm(x)

# Create a data frame with the x and y values
data_df <- data.frame(x, y)

# Calculate the 2.5% and 97.5% quantiles
q_2.5 <- qnorm(0.025)
q_97.5 <- qnorm(0.975)

# Add a column to the data frame indicating which points are at the quantiles
data_df$quantile <- ifelse(data_df$x >= q_2.5 & data_df$x <= q_97.5, "Quantile", "")

# Plot the density
p <- ggplot(data_df, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quantile), alpha = 0.2, color = NA) +
  scale_fill_manual(values = c("Quantile" = "red"), guide = FALSE)

add_dot <- function(p) {
  # Generate a random sample from a standard normal distribution
  x <- rnorm(1)
  
  # Add a new point to the plot
  if(x > q_97.5 | x < q_2.5){
    p <<- p + geom_point(x = x, y = 0, size = 2.5, color = "red")  
  } else {
    p <<- p + geom_point(x = x, y = 0, size = 2.5, color = "blue") 
  }
p
}

add_dot(p)




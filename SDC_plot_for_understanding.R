# SDC plot for better understanding

library(tidyverse)

x <- seq(-3, 3, length.out = 1000)
y <- dnorm(x)

data_df <- data.frame(x, y)

q_2.5 <- qnorm(0.025)
q_97.5 <- qnorm(0.975)

data_df$quantile <- ifelse(data_df$x >= q_2.5 & data_df$x <= q_97.5, "Quantile", "")

p <- ggplot(data_df, aes(x = x, y = y)) +
  geom_line() +
  geom_ribbon(aes(ymin = 0, ymax = y, fill = quantile), alpha = 0.2, color = NA) +
  scale_fill_manual(values = c("Quantile" = "red"), guide = FALSE)

add_dot <- function(p) {
  x <- rnorm(1)
  if(x > q_97.5 | x < q_2.5){
    p <<- p + geom_point(x = x, y = 0, size = 2.5, color = "red")  # global variable
  } else {
    p <<- p + geom_point(x = x, y = 0, size = 2.5, color = "blue") 
  }
p
}

add_dot(p) # execute repeatedly. In approx. every 20th attempt, we would get
# a falsely detected difference.




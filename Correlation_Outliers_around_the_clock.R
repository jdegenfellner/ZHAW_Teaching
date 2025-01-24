# Load necessary libraries
library(ggplot2)
library(gganimate)
library(ggpubr)
library(glue)  # For dynamic text in ggplot

# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 100  # Sample size
rho <- 0.75  # Desired correlation
num_frames <- 100  # Number of frames in the animation
circle_radius <- 8  # Radius for the circle

# Generate bivariate normal data with specified correlation
x <- rnorm(n)
y <- rho * x + sqrt(1 - rho^2) * rnorm(n)

# Store original data
data_original <- data.frame(x = x, y = y)

# Create a sequence of angles for the outlier to move around a circle
angles <- seq(0, 2 * pi, length.out = num_frames)

# Create data for animation
animated_data <- do.call(rbind, lapply(1:num_frames, function(i) {
  # Circular coordinates for the outlier
  x_outlier <- circle_radius * cos(angles[i])
  y_outlier <- circle_radius * sin(angles[i])
  
  # Update data with new outlier position
  data_with_outlier <- data_original
  data_with_outlier$x[n] <- x_outlier
  data_with_outlier$y[n] <- y_outlier
  data_with_outlier$frame <- i  # Add frame indicator
  
  # Calculate the updated correlation for each frame
  data_with_outlier$correlation <- cor(data_with_outlier$x, data_with_outlier$y)
  data_with_outlier
}))

# Create animated plot with dynamic correlation inside the plot area
animated_plot <- ggplot(animated_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(title = "Effect of a Moving Outlier on Correlation and Regression Line",
       x = "X", y = "Y") +
  geom_text(aes(x = -6, y = 7, 
                label = glue("Correlation: {round(correlation, 2)}")),  # Use glue for dynamic text
            size = 5, color = "darkred") +  # Correlation text inside plot
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  transition_manual(frame)  # Animate based on frame

# Render the animation
animate(animated_plot, nframes = num_frames, fps = 10, width = 600, height = 400)
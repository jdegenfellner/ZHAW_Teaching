# GPT-4
# "Produce R-code which generates random images in the style of 
# the painter Kandinsky"

#install.packages("ggplot2")
#install.packages("grid")
library(ggplot2)
library(grid)

generate_kandinsky <- function(seed = 42, width = 800, height = 800, n_shapes = 50) {
  set.seed(seed)
  
  # Define the canvas
  plot <- ggplot() +
    theme_void() +
    theme(plot.margin = margin(0, 0, 0, 0))
  
  # Generate random shapes
  for (i in 1:n_shapes) {
    shape <- sample(c("circle", "rect", "line"), 1)
    color <- rgb(runif(1), runif(1), runif(1), maxColorValue = 1)
    size <- runif(1, 0.01, 0.1)
    xpos <- runif(1)
    ypos <- runif(1)
    
    if (shape == "circle") {
      plot <- plot +
        annotate("path",
                 x = xpos, y = ypos,
                 path = circleGrob(r = size),
                 color = color,
                 size = 2)
    } else if (shape == "rect") {
      plot <- plot +
        annotate("rect",
                 xmin = xpos - size / 2, xmax = xpos + size / 2,
                 ymin = ypos - size / 2, ymax = ypos + size / 2,
                 color = color,
                 size = 2)
    } else if (shape == "line") {
      angle <- runif(1, 0, 2 * pi)
      xend <- xpos + cos(angle) * size
      yend <- ypos + sin(angle) * size
      
      plot <- plot +
        geom_segment(aes(x = xpos, y = ypos, xend = xend, yend = yend),
                     color = color, size = 2)
    }
  }
  
  # Save the image
  ggsave(filename = paste0("kandinsky_", seed, ".png"), plot = plot, width = width / 100, height = height / 100, dpi = 100)
  
  # Return the plot object
  return(plot)
}

kandinsky_plot <- generate_kandinsky(seed = 123, width = 800, height = 800, n_shapes = 50)
kandinsky_plot

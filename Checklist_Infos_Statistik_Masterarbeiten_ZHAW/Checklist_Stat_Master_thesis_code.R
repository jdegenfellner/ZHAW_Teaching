# R-Code for latex-Document: Checklist_Stat_Master_thesis.tex

# Set working directory----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #  to source file location

# Packages----
library(pacman) # installs the package if necessary and loads it for use. 
pacman::p_load(
  broom,
  car,
  tidyverse,
  plotly,
  vcd,
  lme4,
  brms,
  tictoc,
  geepack,
  merTools,
  Hmisc,
  lmerTest,
  MuMIn,
  performance,
  nlme,          # inlcudes gls(), generalized least squares regression
  bestNormalize,
  gstat, # includes variogram()
  quantreg,
  visdat,
  twopartm, 
  hrbrthemes, 
  ggplotify,
  ggpubr
)



# Boxplots vs. nicer ones----
set.seed(123)

# Create sample data
normal_data <- rnorm(100, mean = 20, sd = 5)
outliers <- c(40, 45, 50)
data <- c(normal_data, outliers)
df <- data.frame(var = data)

# Create a simple boxplot
ggplot(df, aes(x = factor(1), y = var)) +
  geom_boxplot() +
  xlab('')

# Create a violin plot with jitter
p1 <- ggplot(df, aes(x = factor(1), y = var)) +
  geom_violin() +
  geom_boxplot(width=0.1) +  # Adds a boxplot inside the violin plot
  geom_jitter(width = 0.2) +
  xlab('') + ylab('Variable-value') + 
  coord_flip() + 
  ggtitle("Example of violin plot with raw data using jitter") + 
  theme(plot.title = element_text(hjust = 0.5))
p1
ggsave(filename = "./images/violin_plot.pdf", plot = p1)

pdf(file = "./images/boxplot.pdf")
boxplot(df$var, horizontal=TRUE, main="Basic R boxplot")
dev.off()

# Histogram vs. violin plot plus box plot----

set.seed(123)
gamma_values <- rgamma(100, shape = 2, rate = 0.5)
df <- data.frame(value = gamma_values)

pdf(file = "./images/histogram.pdf")
hist(df$value, main="Basic R boxplot")
dev.off()

p2 <- ggplot(df, aes(x = value)) +
  geom_histogram(aes(y = ..density..), bins = 30, alpha = 0.7) +
  geom_density(aes(y = ..density..), color = "blue") +
  geom_boxplot(aes(y = -0.03, x = value), width = 0.02, position = position_nudge(y = -0.00)) +
  geom_point(aes(y = -0.03), position = position_jitter(width = 0.002, height = 0.01), size = 1) +
  ggtitle("Histogram with density plot and boxplot below") +
  theme(plot.title = element_text(hjust = 0.5))
p2
ggsave(filename = "./images/density_plot_with_boxplot.pdf", plot = p2)


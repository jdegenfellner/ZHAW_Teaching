# # Nice video:
# Mike Pound: https://www.youtube.com/watch?v=TJdH6rPA-TI&ab_channel=Computerphile
# https://www.youtube.com/watch?v=KtRLF6rAkyo&t=517s&ab_channel=Computerphile

# PCA is basically a coordinate transformation
# One can use PCA to reduce the dimensionality of data by just omitting
# components with low variance.

# In WIKI (https://en.wikipedia.org/wiki/Principal_component_analysis),
# T= X*W
# X is the original data, in our case, data from a bivariate normal distribution
# Task: Find W in order to maximize variance in the first direction (PC1),
# and then in the second direction (PC2) which is orthogonal to PC1, and so forth

library(pacman)
p_load(MASS, tidyverse, ggbiplot, dimensio)

# Create some data-------------
n <- 200
rho <- 0.65
mu <- c(0, 0)
Sigma <- matrix(c(1, rho, rho, 1), ncol = 2)

# Generate data from a bivariate normal distribution
#set.seed(123)  # For reproducibility
data <- mvrnorm(n = n, mu = mu, Sigma = Sigma)
df <- data.frame(x = data[,1], y = data[,2])
plot(df$x, df$y, main = "Bivariate Normal Distribution", xlab = "x", ylab = "y")


# Perform PCA---------
pca <- prcomp(df, center = TRUE, scale. = TRUE) 
# -> scale. = TRUE means we standardize the data -> this way a variable with a huge range does not dominate
# -> center = TRUE means we move the point cloud to the origin (mean of the data is 0)
summary(pca) # shows proportions of variance explained by the principal components

# check orthogonality: 
sum(pca$rotation[,1] * pca$rotation[,2]) # = 0, check
# these are the so-called LOADINGS (W):
pca$rotation
pca$rotation[,1] 
pca$rotation[,2]



# So the algorithm found a valid transformation # W = pca$rotation
# Let's apply this transformation:
# T = X*W =
(T = scale(as.matrix(df)) %*% t(pca$rotation[,1:2])) # scale = TRUE, center = TRUE
head(pca$x)
head(T) # identical!
# -> these are the so-called SCORES (T)
# a matrix multiplication is identical to a linear transformation.
# hence, the PCA just transforms the data linearly under given 
# constraints (find orthogonal max variance directions)
t(pca$rotation[,1:2])
pca$rotation[,1:2] # identical

# we could also plot the points in the new coordinates:
plot(T[,1], T[,2], main = "PCA Transformed Data", xlab = "PC1", ylab = "PC2")
# as you can see, the data is now centered around the origin (0,0)
# and the points have no correlation, this was a goal of pca, to find orthogonal max variance directions
cor(T[,1], T[,2]) # \approx 0, check


# Visualize PCs in the raw data----------

# Extract principal components (loadings scaled by standard deviations)
pc1 <- pca$rotation[,1] * pca$sdev[1] # rotation shows the direction, sdev the variability of the PC
pc2 <- pca$rotation[,2] * pca$sdev[2]

# Compute the center (mean of the data)
center <- colMeans(df)

# Create a data frame for the PCA arrows
pca_arrows <- data.frame(
  x = center[1],
  y = center[2],
  xend = center[1] + pc1[1],
  yend = center[2] + pc1[2]
)

# Add the second principal component
pca_arrows <- rbind(
  pca_arrows,
  data.frame(
    x = center[1],
    y = center[2],
    xend = center[1] + pc2[1],
    yend = center[2] + pc2[2]
  )
)

# Plot the scatter plot with principal component vectors
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_segment(data = pca_arrows, 
               aes(x = x, y = y, xend = xend, yend = yend),
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "red", linewidth = 1.2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  coord_equal() +
  theme_minimal() +
  labs(title = "PCA: Principal Components of a Bivariate Normal Distribution",
       x = "x", y = "y") + 
  theme(plot.title = element_text(hjust = 0.5))

# -> Note that the line of the simple regression (dashed blue) is not
#    identical to the first principal component. the principal components
#    are orthogonal to each other and point towards the directions of maximum variance
#    This is not true for the linear regression, which optimizes predictive ability, i.e. VERTICAL distances.
#    PCA minimizes ORTHOGONAL distances from the line to the points.


# Reconstruct original data from PCA components----------
# Extract means and standard deviations used for scaling

# Step 1: Reconstruct standardized (centered + scaled) data
reconstructed_scaled <- pca$x %*% t(pca$rotation) # follows from T=X_centered*W -> X_centered=W^(-1)*T

# Step 2: Multiply each column by its corresponding original standard deviation
reconstructed_unscaled <- reconstructed_scaled
reconstructed_unscaled[,1] <- reconstructed_scaled[,1] * sd(df$x)
reconstructed_unscaled[,2] <- reconstructed_scaled[,2] * sd(df$y)

# Step 3: Add back the original means
reconstructed <- reconstructed_unscaled
reconstructed[,1] <- reconstructed_unscaled[,1] + mean(df$x)
reconstructed[,2] <- reconstructed_unscaled[,2] + mean(df$y)

# Compare original and reconstructed
head(df)
head(reconstructed)
# -> identical


# PCA explains the following proportion of variance:--------
pca$sdev[1]^2/(pca$sdev[1]^2 + pca$sdev[2]^2) # 0.8

EIG <- eigen(t(as.matrix(df))%*%as.matrix(df))$values
# explained variance:
EIG[1]/(EIG[1] + EIG[2]) #\approx 0.8, first PC explains 80% of the variance

screeplot(pca, main = "Scree Plot of PCA") # Scree plot to visualize variance explained by each component

# Biplot----------
# Samples are displayed as points while variables are 
# displayed either as vectors
# The “bi” in biplot refers to the fact that two sets of points (i.e.,
# the rows and columns of the data matrix) are visualized by scalar 
# products, not the fact that the display is usually two-dimensional.
# https://en.wikipedia.org/wiki/Biplot
biplot(pca, main = "Biplot of PCA") # 
biplot(pca, var.axes = FALSE)

ggbiplot(pca, main = "Biplot of PCA") + 
  theme_minimal() +
  labs(title = "Biplot of PCA", x = "PC1", y = "PC2") +
  theme(plot.title = element_text(hjust = 0.5))

# https://cran.r-project.org/web/packages/dimensio/vignettes/pca.html
X <- dimensio::pca(df, center = TRUE, scale = TRUE, rank = 2)
plot(df$x, df$y, main = "Bivariate Normal Distribution", xlab = "x", ylab = "y")
X@dimension # 1?
X@singular_values
X@columns
get_eigenvalues(X)
dimensio::screeplot(X, cumulative = TRUE, limit = 2) # ERROR? only 1 dim
biplot(X, type = "form", labels = "variables") # ERROR


# Cosine of the angle between the vectors is the correlation between the variables----------
cor(df$x, df$y) #\approx 0.65

# Compute the coordinates of the variable arrows in the biplot
coords_vars <- pca$rotation[, 1:2] %*% diag(pca$sdev[1:2])

# Get the two vectors (x and y arrows in the biplot)
vec_x <- coords_vars["x", ]
vec_y <- coords_vars["y", ]

# Compute the cosine of the angle between them
cos_theta <- sum(vec_x * vec_y) / (sqrt(sum(vec_x^2)) * sqrt(sum(vec_y^2)))

# Actual correlation between original x and y
cor_xy <- cor(df$x, df$y)

# Output
cat("Cosine of angle between variable arrows (biplot):", round(cos_theta, 4), "\n")
cat("Pearson correlation between x and y:", round(cor_xy, 4), "\n")


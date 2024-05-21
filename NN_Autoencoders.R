# Autoencoder

# GPT-4: I want to show the principle of Autoencoder networks to my 
# students using R. Could you create a nice example plus vizualization?

# Install necessary packages
#install.packages("keras")
#install.packages("ggplot2")
#install.packages("gridExtra")

# TODO DEBUG

# Load necessary libraries
library(pacman)
p_load(keras, tidyverse, gridExtra,
       tensorflow, tictoc, reticulate)

#py_install("tensorflow")
py_run_string("import tensorflow as tf")
py_run_string("print(tf.__version__)")

# Load the mnist dataset
mnist <- dataset_mnist()
x_train <- mnist$train$x
x_test <- mnist$test$x

dim(x_train)  # Should be (60000, 28, 28)
dim(x_test)   # Should be (10000, 28, 28)

# Reshape and normalize
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
x_train <- x_train / 255
x_test <- x_test / 255

# Define the encoder
input_img <- layer_input(shape = c(784))
encoded <- input_img %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 32, activation = 'relu')

# Define the decoder
decoded <- encoded %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dense(units = 784, activation = 'sigmoid')

# Combine the encoder and the decoder into an autoencoder model
autoencoder <- keras_model(input = input_img, output = decoded)

# Compile the model
#autoencoder %>% compile(optimizer = 'adadelta', loss = 'binary_crossentropy') # Does not result in nice reconstructions...
autoencoder %>% compile(optimizer = optimizer_adadelta(learning_rate = 1.0), loss = 'mean_squared_error')

# Train the model
tic()
autoencoder %>% fit(x_train, x_train, 
                    epochs = 250, 
                    batch_size = 256,
                    shuffle = TRUE,
                    validation_data = list(x_test, x_test))
toc() # 389.137 sec elapsed

# Use the trained model to make predictions on the test data
decoded_imgs <- autoencoder %>% predict(x_test)

# Plot the original and reconstructed images
par(mfrow = c(2, 10), mai = c(0.2, 0.2, 0.2, 0.2))
for(i in 1:10) {
  # Original
  img <- array_reshape(x_test[i,], c(28, 28))
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n', main = 'Original')
  
  # Reconstruction
  img <- array_reshape(decoded_imgs[i,], c(28, 28))
  image(1:28, 1:28, img, col = gray((0:255)/255), xaxt = 'n', yaxt = 'n', main = 'Reconstructed')
}

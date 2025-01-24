library(nnet)
library(tidyverse)
library(ggalt)
library(neuralnet)
data(iris)

#[this script was partially created using GPT4]

# In this example, we used the nnet package to train a neural network with 
# a single hidden layer of 10 neurons to classify the iris dataset. The 
# data was split into training and testing sets (70% and 30%, respectively),
# and the model was trained on the training set. Finally, we evaluated the
# accuracy of the model on the test set.

# Prepare------------

set.seed(123)
shuffled_iris <- iris[sample(nrow(iris)), ]

# Split the dataset into training (70%) and testing (30%) sets
train_index <- sample(1:nrow(shuffled_iris), 0.7 * nrow(shuffled_iris))
train_set <- shuffled_iris[train_index, ]
test_set <- shuffled_iris[-train_index, ]

# Set the seed for reproducibility
set.seed(123)

# Train ---------------
# Train the neural network using the nnet function
nn_model <- nnet(Species ~ ., data = train_set, size = 10, # size: units in the hidden layer
                 rang = 0.1, decay = 5e-4, maxit = 200)

# Predict----
# Make predictions using the trained neural network
predicted_species <- predict(nn_model, test_set, type = "class")

# Calculate the accuracy
accuracy <- sum(predicted_species == test_set$Species) / nrow(test_set)
cat("Accuracy:", accuracy)

# Add the predicted species to the test_set dataframe
test_set$PredictedSpecies <- predicted_species

# Visualize-------- 
# _classifications----------
# Filter the misclassified points
misclassified <- test_set[test_set$Species != test_set$PredictedSpecies,]

# Create a ggplot object with Sepal.Length and Sepal.Width as the x and y axis
p <- ggplot(test_set, aes(x = Sepal.Length, y = Sepal.Width))

# Add jitter and alpha to geom_point for better visualization
p <- p + geom_jitter(aes(color = Species, shape = PredictedSpecies), size = 4, alpha = 0.7, width = 0.1, height = 0.1)

# Add a red circle around the misclassified observation
p <- p + geom_encircle(data = misclassified, aes(x = Sepal.Length, y = Sepal.Width), color = "red", size = 1, expand = 0.1)


# Customize the plot appearance
p <- p + labs(title = "Neural Network Classification of Iris Dataset",
              x = "Sepal Length",
              y = "Sepal Width",
              color = "True Species",
              shape = "Predicted Species") +
  theme_minimal()

print(p)

# _net----

# Prepare the data
train_set_nn <- train_set
train_set_nn$Species <- as.numeric(train_set_nn$Species) - 1

# Train the neural network
nn_model_neuralnet <- neuralnet(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                                data = train_set_nn, hidden = 10, linear.output = FALSE, threshold = 0.01)

# Plot the neural network
plot(nn_model_neuralnet, rep = "best")


# Compare multinomial logistic regression----
multinom_model <- nnet::multinom(Species ~ ., data = train_set)
summary(multinom_model)

# Make predictions using the multinomial logistic regression model
predicted_species_multinom <- predict(multinom_model, test_set)

# Calculate the accuracy
accuracy_multinom <- sum(predicted_species_multinom == test_set$Species) / nrow(test_set)
cat("Multinomial Logistic Regression Accuracy:", accuracy_multinom)


# Compare to normal linear regression -----
# Recode the Species variable to be numerical in both train_set and test_set
train_set_num <- train_set
train_set_num$Species <- as.numeric(train_set_num$Species) - 1

test_set_num <- test_set
test_set_num$Species <- as.numeric(test_set_num$Species) - 1

# Train the linear regression model
lin_reg_model_num <- lm(Species ~ ., data = train_set_num)

# Make predictions using the linear regression model
predicted_species_lin_reg_num <- predict(lin_reg_model_num, test_set_num)

# Convert the continuous predictions to the closest class
predicted_species_lin_reg_num_class <- as.factor(round(predicted_species_lin_reg_num))

# Calculate the accuracy
accuracy_lin_reg_num <- sum(predicted_species_lin_reg_num_class == test_set_num$Species) / nrow(test_set_num)
cat("Linear Regression (numerical response) Accuracy:", accuracy_lin_reg_num)

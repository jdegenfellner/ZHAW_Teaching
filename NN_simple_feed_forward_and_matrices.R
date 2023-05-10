# Simple feed forward networks without activation functions are equivalent to
# Matrix operations (multiplication and addition):


# Initial (not the only) instruction to GPT-4:
# "I want to show my students that a simple feed forward network without 
# activation functions is equivalent to matrix operations.
# So, let's make up a neural network with 3 input nodes, 2 hidden layers 
# with 3 and 4 neurons and an output layer with 2 nodes. 
# Just invent weights and biases for all the nodes.
# Show the equivalence in R and also plot the corresponding neural nets and 
# matrices for us."

# Define the weights and biases
W1 <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), nrow=3)
W2 <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2), nrow=4)
W3 <- matrix(c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8), nrow=2)

b1 <- matrix(c(0.1, 0.2, 0.3), nrow=3)
b2 <- matrix(c(0.1, 0.2, 0.3, 0.4), nrow=4)
b3 <- matrix(c(0.1, 0.2), nrow=2)

# Define the input
X <- matrix(c(1, 2, 3), nrow=3)

# Perform the feed-forward operation
H1 <- W1 %*% X + b1
H2 <- W2 %*% H1 + b2
Output <- W3 %*% H2 + b3

library(DiagrammeR)

# Define the weights and biases as vectors for easy access
weights <- c(W1, W2, W3)
biases <- c(b1, b2, b3)

# Create a blank graph
g <- create_graph() %>%
  add_n_nodes(n = 3, type = "input") %>%
  add_n_nodes(n = 3, type = "hidden1") %>%
  add_n_nodes(n = 4, type = "hidden2") %>%
  add_n_nodes(n = 2, type = "output")

# Add edges between the nodes with weights as labels
edge_index = 1
for(i in 1:3){
  for(j in 4:6){
    g <- g %>% add_edge(from = i, to = j, edge_aes = edge_aes(label = weights[edge_index]))
    edge_index = edge_index + 1
  }
}

for(i in 4:6){
  for(j in 7:10){
    g <- g %>% add_edge(from = i, to = j, edge_aes = edge_aes(label = weights[edge_index]))
    edge_index = edge_index + 1
  }
}

for(i in 7:10){
  for(j in 11:12){
    g <- g %>% add_edge(from = i, to = j, edge_aes = edge_aes(label = weights[edge_index]))
    edge_index = edge_index + 1
  }
}

# Render the graph
render_graph(g)


# Get the weights associated with the connections to node 6
weights_to_node6 <- W1[3, ]  # 3rd row of W1

# Compute the dot product of the inputs and these weights
input_to_node6 <- sum(X * weights_to_node6)

# Add the bias for node 6
(value_of_node6 <- input_to_node6 + b1[3])


# manually:
1*0.3 + 2*0.6 + 3*0.9 + b1[3] # check


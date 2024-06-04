# NN (Euclidean) distances in high dimensions:

n <- 1100 # dimensions
num_sim <- 1000 # number of draws

# Note: num_sim needs to be >= n since the for loop below only runs until num_sim

# Let's draw random vectors from the unit cube [0,1]^n

length_of_unit_vector <- norm(rep(1,n), type="2")

distances <- rep(0, n)
for(i in 1:num_sim){
  x1 <- runif(n)
  x2 <- runif(n)
  distances[i] <- norm(x1 - x2, type = "2")
}

hist(distances)
summary(distances)
sum(distances == 0) # exactly 100! Why?
# This happens as soon as n > num_sim and gives exactly the difference zeros.




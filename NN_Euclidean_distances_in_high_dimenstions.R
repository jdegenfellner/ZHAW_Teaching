# NN (Euclidean) distances in high dimensions:

n <- 6000 # dimensions
num_sim <- 10000 # number of draws

# Let's draw random vectors from the unit cube [0,1]^n

length_of_unit_vector <- norm(rep(1,n), type="2")

distances <- rep(0, n)
for(i in 1:num_sim){
  x1 <- runif(n)
  x2 <- runif(n)
  distances[i] <- norm(x1 - x2, type = "2")
}

hist(distances)


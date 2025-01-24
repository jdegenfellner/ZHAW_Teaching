# in progress # 

library(pacman)
p_load(random, tictoc)

tic()
random_numbers <- randomNumbers(n = 100000, min = 0, max = 3, col = 1)
random_numbers
toc()
# Error in randomNumbers(n = 1e+05, min = 0, max = 3, col = 1) : 
# Random number requests must be between 1 and 10,000 numbers
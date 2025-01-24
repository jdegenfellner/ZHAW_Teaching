# Sample size calculation via simulation
# for Correlation coefficient

# Jan 25

# We assume that the relationship between the variables X and Y
# can be reasonable well described using a linear function.

# Target correlation
rho <- 0.5

# Precision with which we want to know the correlation
delta <- 0.15

# One way to search is to demand:--------
# The true but unknown correlation should lie within the interval
# [rho-delta, rho+delta]


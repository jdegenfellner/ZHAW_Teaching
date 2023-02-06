# Folie BMI, Masse, Groesse sind nicht linear unabhaengig...

library(pracma)

# using: # https://web.maths.unsw.edu.au/~danielch/linear12/lecture14.pdf
# Vectors are linearly independent if the kernel is trivial

height <- rnorm(100, mean = 165, sd = 13)
mass <- rnorm(100, mean = 75, sd = 10)
bmi <- mass/(height/100)^2
outcome <- rnorm(100, mean = 23, sd = 2)
df <- data.frame(outcome = outcome, height = height, mass = mass, bmi = bmi)
df_mat <- as.matrix(df)
nullspace(df_mat) # kernel trivial
det(t(df_mat) %*% df_mat) # ?
detect.lindep(df) # "No linear dependent column(s) detected."
lm(outcome ~ height + mass + bmi, data = df) # estimator for bmi exists.


bmi <- 2*height + 3*mass
df <- data.frame(outcome = outcome, height = height, mass = mass, bmi = bmi)
df_mat <- as.matrix(df)
nullspace(df_mat) # kernel not trivial
det(t(df_mat) %*% df_mat) # ?
detect.lindep(df) # "Suspicious column name(s):   height, mass, bmi"
lm(outcome ~ height + mass + bmi, data = df) # estimator for bmi does not exist.

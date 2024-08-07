# Folie BMI, Masse, Groesse sind nicht linear unabhaengig...

library(pacman)
p_load(pracma, plm)

# using: # https://web.maths.unsw.edu.au/~danielch/linear12/lecture14.pdf
# Vectors are linearly independent if the kernel is trivial

height <- rnorm(100, mean = 165, sd = 13)
mass <- rnorm(100, mean = 75, sd = 10)
bmi <- mass/(height/100)^2
outcome <- rnorm(100, mean = 23, sd = 2)
df <- data.frame(outcome = outcome, height = height, mass = mass, bmi = bmi)
df_mat <- as.matrix(df)
nullspace(df_mat) # kernel trivial
det(t(df_mat) %*% df_mat) # != 0
solve(t(df_mat) %*% df_mat) # Inverse exists, rather close to zero-matrix
round(solve(t(df_mat) %*% df_mat))
detect.lindep(df) # "No linear dependent column(s) detected."
lm(outcome ~ height + mass + bmi, data = df) # estimator for bmi exists.


# Now we define a linear dependence among the variables:
bmi <- 2*height + 3*mass
df <- data.frame(outcome = outcome, height = height, mass = mass, bmi = bmi)
df_mat <- as.matrix(df)
nullspace(df_mat) # kernel not trivial
det(t(df_mat) %*% df_mat) # != 0 and large
# https://www.math.uni-duesseldorf.de/~internet/WiWi_WS1415/inversematrizen.pdf
# "Satz 8 Eine quadratische Matrix A ist genau dann invertierbar, wenn det(A) != 0."
solve(t(df_mat) %*% df_mat) # BUT: inverse does not exist
# What happened?
# Since det(A^(-1)) = 1/det(A), this means that the determinant of the inverse
# is close to zero and therefore almost singular!
detect.lindep(df) # "Suspicious column name(s):   height, mass, bmi"
lm(outcome ~ height + mass + bmi, data = df) # Estimator for bmi does not exist.

# Height, mass and bmi are linearly (!) independent since bmi is NOT a linear (!)
# combination of the vectors height and mass. 


library(pacman)
p_load(rFSA, tidyverse)

data(mtcars)
mtcars_tbl <- as_tibble(mtcars)

# Define the response variable and predictor variables
response <- "mpg"
predictors <- c("cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb")

# Use the FSA function to find the best subset model
# We'll limit to models with up to 3 predictors and include interactions up to the 2nd order
results <- FSA(
  formula = as.formula(paste(response, "~ 1")), 
  data = mtcars_tbl, 
  fitfunc = lm, 
  fixvar = NULL, # covariates that should always be included
  quad = TRUE, # include quadratic terms or not
  m = 2, # order of terms
  numrs = 10, # number of random starts to perform
  interactions = TRUE, # whether to include interactions in model
  cores = 1 # number of cores to use while running
)

print(results)

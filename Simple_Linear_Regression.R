# Some code for simple linear regression
# From https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Simple_Linear_Regression.R
# 24.1.24
# QM2 
# See also Script QM2 (Meichtry, S.2-13)
# See also https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Simple_linear_regression_shiny.R

# Example data for simple linear regression----
# https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regressionSimple.csv
# https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regression.csv

library(pacman)
p_load(plotly, tidyverse)

# ad page 5 Script ----
# Let's look at formula (1.2.6) and use data from above
df <- read.csv("https://raw.githubusercontent.com/jdegenfellner/ZHAW_Teaching/main/Data/regressionSimple.csv")
str(df)
mod <- lm(y ~ x, data = df) # least squares method.
summary(mod)

sex <- c("male", "female")

vec_sex <- sample(sex, 100, replace = TRUE)
str(vec_sex)
vec_sex_f <- factor(vec_sex) # Levels: female male

# define other reference level for factor:
vec_sex_f <- relevel(vec_sex_f, ref = "male")
vec_sex_f

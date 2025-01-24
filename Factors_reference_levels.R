sex <- c("male", "female")
vec_sex <- sample(sex, 100, replace = TRUE)
str(vec_sex)
vec_sex_f <- factor(vec_sex) # Levels: female male
levels(vec_sex_f) # reference level: "female"

# Define other reference level for factor:
vec_sex_f <- relevel(vec_sex_f, ref = "male")
vec_sex_f



# What about ordered factors?
rating <- c("AAA", "AA", "A", "BBB", "AA", "BBB", "A")
ordered(rating, levels = c("AAA", "AA", "A", "BBB"))
rating[1]
rating[2]
rating[2] < rating[1] # TRUE

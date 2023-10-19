# Cronbach's Alpha, tau-äquivalente Reliabilität

# in progresss # 

# Note: There are other packages with Cronbach's alpha:
# https://search.r-project.org/CRAN/refmans/DescTools/html/CronbachAlpha.html
# https://www.rdocumentation.org/packages/psych/versions/2.3.6/topics/alpha

library(ltm)

# Enter survey responses as a data frame
data <- data.frame(Q1 = c(1, 2, 2, 3, 2, 2, 3, 3, 2, 3),
                   Q2 = c(1, 1, 1, 2, 3, 3, 2, 3, 3, 3),
                   Q3 = c(1, 1, 2, 1, 2, 3, 3, 3, 2, 3))

# Calculate Cronbach's Alpha
cronbach.alpha(data) # 0.773


# Calculate manually:
# https://de.wikipedia.org/wiki/Cronbachsches_Alpha

# Formula 1:----
# alpha = N*r_bar/(1+(N-1)*r_bar)

N <- dim(data)[2]

cor1 <- cor(data$Q1, data$Q2)
cor2 <- cor(data$Q1, data$Q3)
cor3 <- cor(data$Q2, data$Q3)
r_bar <- mean(c(cor1,cor2,cor3))

N*r_bar/(1+(N-1)*r_bar) # 0.774291 # not too bad. small error?


# Formula 2:----
# alpha = N/(N-1)*(1-sum(sigma_Yi^2)/sigma_X^2)

N <- 3
data <- data %>% mutate(sum_score = Q1 + Q2 + Q3)
sigma_X <- sqrt(var(data$sum_score))

sum_Yi_sq <- var(data$Q1) + var(data$Q2) + var(data$Q3)

N/(N-1)*(1-sum_Yi_sq/sigma_X^2) # 0.7734375


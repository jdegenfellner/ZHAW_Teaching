# Cronbach's Alpha, tau-äquivalente Reliabilität

# in progresss # 

library(ltm)

#enter survey responses as a data frame
data <- data.frame(Q1 = c(1, 2, 2, 3, 2, 2, 3, 3, 2, 3),
                   Q2 = c(1, 1, 1, 2, 3, 3, 2, 3, 3, 3),
                   Q3 = c(1, 1, 2, 1, 2, 3, 3, 3, 2, 3))

#calculate Cronbach's Alpha
cronbach.alpha(data) # 0.773


# Calculate manually:
# https://de.wikipedia.org/wiki/Cronbachsches_Alpha

# Formula 1:
# N*r_bar/(1+(N-1)*r_bar)

N <- dim(data)[2]

cor1 <- cor(data$Q1, data$Q2)
cor2 <- cor(data$Q1, data$Q3)
cor3 <- cor(data$Q2, data$Q3)
r_bar <- mean(c(cor1,cor2,cor3))

N*r_bar/(1+(N-1)*r_bar) # 0.774291 # not too bad.

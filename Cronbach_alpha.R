# Cronbach's Alpha, tau-äquivalente Reliabilität

# in progresss # 

library(ltm)

#enter survey responses as a data frame
data <- data.frame(Q1 = c(1, 2, 2, 3, 2, 2, 3, 3, 2, 3),
                   Q2 = c(1, 1, 1, 2, 3, 3, 2, 3, 3, 3),
                   Q3 = c(1, 1, 2, 1, 2, 3, 3, 3, 2, 3))

#calculate Cronbach's Alpha
cronbach.alpha(data)


# Calculate manually:
# https://de.wikipedia.org/wiki/Cronbachsches_Alpha

# Formula 1:
# N*r_bar/(1+(N-1)*r_bar)

N <- dim(data)[1]

# Cronbach's Alpha, tau-äquivalente Reliabilität

# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Cronbach_alpha.R

# in progress # 

# Note: There are different packages with Cronbach's alpha:
# https://www.rdocumentation.org/packages/ltm/versions/1.2-0/topics/cronbach.alpha
# https://search.r-project.org/CRAN/refmans/DescTools/html/CronbachAlpha.html
# https://www.rdocumentation.org/packages/psych/versions/2.3.6/topics/alpha

library(pacman)
p_load(ltm, psych, DescTools)

# Enter survey responses as a data frame
data <- data.frame(Q1 = c(1, 2, 2, 3, 2, 2, 3, 3, 2, 3),
                   Q2 = c(1, 1, 1, 2, 3, 3, 2, 3, 3, 3),
                   Q3 = c(1, 1, 2, 1, 2, 3, 3, 3, 2, 3))
data
# Calculate Cronbach's Alpha----
# ltm-package
cronbach.alpha(data) # 0.773
# psych-package
res <- psych::alpha(data)
res # shows (among others):
# raw_alpha - alpha based upon the covariances
# std.alpha - The standarized alpha based upon the correlations
# G6(smc) - Guttman's Lambda 6 reliability
# average_r - The average interitem correlation
# S/N - Signal/Noise ratio
# ase - average standard error
# mean   
# sd 
# median_r - The median interitem correlation



# Calculate manually:----
# https://de.wikipedia.org/wiki/Cronbachsches_Alpha
# kann Werte zwischen minus unendlich und 1 annehmen (obwohl nur 
# positive Werte sinnvoll interpretierbar sind). Als Faustregel
# sollte ein beliebiges psychometrisches Instrument nur verwendet
# werden, wenn ein Wert fuer alpha von 0,65 oder mehr erreicht wird.
# Als kritisch wird allerdings auch ein sehr hoher Wert (z. B. 0,95) 
# eingeschätzt, da dies darauf hindeutet, dass mehrere Items redundant sind.

# _Formula 1:----
# alpha = N*r_bar/(1+(N-1)*r_bar)

N <- dim(data)[2]

cor1 <- cor(data$Q1, data$Q2)
cor2 <- cor(data$Q1, data$Q3)
cor3 <- cor(data$Q2, data$Q3)
r_bar <- mean(c(cor1, cor2, cor3)) # average inter-item 

N*r_bar/(1+(N-1)*r_bar) # 0.774291 # not too bad. small error?

# try: get average_r from package:
res <- psych::alpha(data)
res$total$average_r
N*res$total$average_r/(1+(N-1)*res$total$average_r) # 0.774291 same

# _Formula 2:----
# alpha = N/(N-1)*(1-sum(sigma_Yi^2)/sigma_X^2)

N <- 3
data <- data %>% mutate(sum_score = Q1 + Q2 + Q3)
sigma_X <- sqrt(var(data$sum_score))

sum_Yi_sq <- var(data$Q1) + var(data$Q2) + var(data$Q3)

N/(N-1)*(1-sum_Yi_sq/sigma_X^2) # 0.7734375


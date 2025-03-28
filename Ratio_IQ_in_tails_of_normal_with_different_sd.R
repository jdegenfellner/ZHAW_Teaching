# Draw two normal densities with mean=100 and sd1=15, sd2=20
mean <- 100
sd1 <- 15
sd2 <- 20
x <- seq(0, 200, length.out = 500)
y1 <- dnorm(x, mean = mean, sd = sd1)
y2 <- dnorm(x, mean = mean, sd = sd2)
plot(x, y1, type = "l", col = "blue", lwd = 2, 
     ylab = "Density", xlab = "x", 
     main = "Normal Densities with Different SDs")
lines(x, y2, col = "red", lwd = 2, lty = 2)
legend("topright", legend = c("SD = 15", "SD = 20"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)

# Ratio in high IQs----------
ratio_men_women_above_given_iq_high <- ( 1-pnorm(seq(100, 200, 1), 
                                            mean = 100, 
                                            sd = 20) )/(1-pnorm(seq(100, 200, 1), 
                                                                mean = 100, 
                                                                sd = 15))
plot(seq(100, 200, 1), ratio_men_women_above_given_iq_high,
     main = "Ratio Men to women", 
     xlab = "IQ",
     ylab = "Ratio",
     type = "l")

# Ratio in low IQs-------
ratio_men_women_above_given_iq_low <- ( pnorm(seq(0, 100, 1), 
                                          mean = 100, 
                                          sd = 20) )/(pnorm(seq(0, 100, 1), 
                                                            mean = 100, 
                                                            sd = 15))
plot(seq(0, 100, 1), ratio_men_women_above_given_iq_low, 
     main = "Ratio Men to women", 
     xlab = "IQ",
     ylab = "Ratio",
     type = "l")

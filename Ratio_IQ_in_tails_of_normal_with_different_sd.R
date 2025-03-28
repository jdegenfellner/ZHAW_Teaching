# https://www.swr.de/wissen/1000-antworten/sind-mehr-maenner-hochbegabt-102.html#:~:text=Zun%C3%A4chst%20sind%20M%C3%A4nner%20im%20Schnitt,nach%20oben%20und%20nach%20unten.
# Of course, the below is simplified should be seen as a toy example.

# Two normals with different sd---------
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
ratio_men_women_above_given_iq_high <- ( 1-pnorm(seq(101, 200, 1), 
                                            mean = 100, 
                                            sd = 20) )/(1-pnorm(seq(101, 200, 1), 
                                                                mean = 100, 
                                                                sd = 15))
plot(seq(101, 200, 1), ratio_men_women_above_given_iq_high,
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

# combine:
plot(seq(0, 200, 1), c(ratio_men_women_above_given_iq_low,
                       ratio_men_women_above_given_iq_high),
     main = "Ratio Men to women", 
     xlab = "IQ",
     ylab = "Ratio",
     type = "l")


# all into one plot:--------
mean <- 100
sd1 <- 15  # women
sd2 <- 20  # men
x_density <- seq(40, 160, length.out = 500)
x_ratio <- seq(0, 200, 1)

# Densities
y1 <- dnorm(x_density, mean = mean, sd = sd1)  # women
y2 <- dnorm(x_density, mean = mean, sd = sd2)  # men

# Ratio of men to women (tail probabilities)
ratio <- (1 - pnorm(x_ratio, mean = mean, sd = sd2)) / 
  (1 - pnorm(x_ratio, mean = mean, sd = sd1))
ratio[x_ratio <= mean] <- pnorm(x_ratio[x_ratio <= mean], mean = mean, sd = sd2) / 
  pnorm(x_ratio[x_ratio <= mean], mean = mean, sd = sd1)

# Adjust y-limits for density (this is the fix!)
ylim_density <- c(0, max(c(y1, y2)) * 1.1)

# Plot setup
par(mar = c(5, 4, 4, 4) + 0.3)  # Extra space for right axis

# Plot densities
plot(x_density, y1, type = "l", lwd = 2, col = "blue",
     xlab = "IQ", ylab = "Density",
     ylim = ylim_density,
     main = "Normal Densities and Men/Women Ratio")
lines(x_density, y2, col = "red", lwd = 2, lty = 2)

# Add ratio on right axis
par(new = TRUE)
plot(x_ratio, ratio, type = "l", lwd = 2, col = "darkgreen",
     axes = FALSE, xlab = "", ylab = "", ylim = c(0, max(ratio) * 1.1))
axis(side = 4, col = "darkgreen", col.axis = "darkgreen", las = 1)
mtext("Ratio (Men/Women)", side = 4, line = 3, col = "darkgreen")
abline(h = 1, col = "gray", lty = 3)

# Legend
legend("topright",
       legend = c("Women (SD=15)", "Men (SD=20)", "Ratio (Men/Women)"),
       col = c("blue", "red", "darkgreen"),
       lty = c(1, 2, 1), lwd = 2)

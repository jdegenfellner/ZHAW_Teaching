# Bsp. Sample size für Korrelation
library(tidyverse)

# Grundsaetzlich interessiert uns immer die Power-SampleSize-Kurve.
library(pwr)
# Das z.B. waere der Code fuer eine Korrelation r von mind. 0.4 als 
# Alternativhypothese, d.h. wenn ich verwerfe, nehme ich an, diese sei
# mind 0.4.
pwr.r.test(r = 0.4, n = 20, sig.level = 0.05, alternative = "greater")

# Jetzt kann man sich nateurlich leicht daraus eine Power-SampleSize-Kurve zeichnen:
powers1 <- rep(NA, length(10:100))
r <- 0.3
for (i in 10:100) {
  pwr_test <- pwr.r.test(r = r, n = i, sig.level = 0.05, alternative = "greater")
  powers1[i-9] <- pwr_test$power 
}

powers2 <- rep(NA, length(10:100))
r <- 0.5
for (i in 10:100) {
  pwr_test <- pwr.r.test(r = r, n = i, sig.level = 0.05, alternative = "greater")
  powers2[i-9] <- pwr_test$power 
}

powers3 <- rep(NA, length(10:100))
r <- 0.7
for (i in 10:100) {
  pwr_test <- pwr.r.test(r = r, n = i, sig.level = 0.05, alternative = "greater")
  powers3[i-9] <- pwr_test$power 
}

df <- data.frame(n = 10:100, power1 = powers1, power2 = powers2, power3 = powers3)
df %>% ggplot(aes(x = n, y = powers1, color = "r=0.3", shape = "r=0.3")) + 
  geom_line(y = powers1) + 
  geom_line(aes(y = powers2, color = "r=0.5", shape = "r=0.5")) + 
  geom_line(aes(y = powers3, color = "r=0.7", shape = "r=0.7")) + 
  scale_y_continuous(breaks = seq(0,1,0.1)) + 
  scale_color_manual(values = c("r=0.3" = "red", "r=0.5" = "blue", "r=0.7" = "green")) + 
  scale_shape_manual(values = c("r=0.3" = 21, "r=0.5" = 22, "r=0.7" = 23)) +
  ylab("Power") + ggtitle("Statistical Power for correlation analysis depending on correlation and sample size") + 
  guides(color = guide_legend(title = "Correlations"),
         shape = guide_legend(title = "Correlations"))

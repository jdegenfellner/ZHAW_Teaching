# Thinking about 
# risk (ratios)
# odds (ratios)

library(tidyverse)

# in progress # 
p1 <- 0.001
odds1 <- p1/(1-p1)
p2 <- 0.002
odds2 <- p2/(1-p2)
(odds_ratio <- odds2/odds1)
(risk_ratio <- p2/p1)


# make more:
n <- 1000
odds2_seq <- risk_ratios <- odds_ratios <- rep(NA, n)
p1 <- 0.1
p2_seq <- seq(from = 0.0001, to = 0.8, length.out = n) 
odds1 <- p1/(1-p1)

for(i in 1:n){
  odds2_seq[i] <- p2_seq[i]/(1 - p2_seq[i])
  odds_ratios[i] <- odds2_seq[i]/odds1
  risk_ratios[i] <- p2_seq[i]/p1
}

df <- data.frame(index = 1:n, 
                 odds_ratios = odds_ratios, 
                 risk_ratios = risk_ratios,
                 rr_abs_diff_to_one = abs(risk_ratios-1),
                 or_abs_diff_to_one = abs(odds_ratios-1))

df %>% ggplot() + 
  geom_line(aes(x = index, y = odds_ratios, colour = "Odds Ratios")) +
  geom_line(aes(x = index, y = risk_ratios, colour = "Risk Ratios")) +
  scale_color_manual(values = c("Odds Ratios" = "blue", "Risk Ratios" = "red")) +
  labs(title = "Odds Ratios and Risk Ratios", 
       x = "Index", 
       y = "Ratios", 
       color = "Ratios") 

df %>% ggplot() + 
  geom_line(aes(x = index, y = rr_abs_diff_to_one, colour = "RR Abs Diff to One")) +
  geom_line(aes(x = index, y = or_abs_diff_to_one, colour = "OR Abs Diff to One")) +
  scale_color_manual(values = c("RR Abs Diff to One" = "blue", "OR Abs Diff to One" = "red")) +
  labs(title = "Absolute Differences to One (Risk Ratios and Odds Ratios)", 
       x = "Index", 
       y = "Absolute Differences", 
       color = "Ratios") 



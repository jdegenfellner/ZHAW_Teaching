# ICC, some thoughts:
seq(0.1, 1, by = 0.1)
n <- c(seq(0.1, 1, by = 0.1), 1:20)
sigma_e <- 1
sigma_p <- sigma_e/n

ICC <- sigma_p/(sigma_p + sigma_e)

df <- data.frame(sigma_p = sigma_p, proportion = n, ICC = ICC)
df %>% ggplot(aes(x = proportion, y = ICC)) + 
  geom_line() + xlab("n: sigma_p = sigma_e/n") + 
  ggtitle("Relationship between error-variances and ICC") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 1, color = "red") + 
  annotate("text", x = 1.7, y = 0.1, label = "n=1", color = "red")

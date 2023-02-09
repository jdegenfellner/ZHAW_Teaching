# ICC, some thoughts:

# How does the ICC behave if we change the factor between the two variances?
n <- c(seq(0.01, 1, by = 0.01), 1:20)
sigma_e <- 1
sigma_p <- sigma_e/n

ICC <- sigma_p/(sigma_p + sigma_e)

df <- data.frame(sigma_p = sigma_p, factor_n = n, ICC = ICC)
df %>% ggplot(aes(x = factor_n, y = ICC)) + 
  geom_line() + xlab("n: sigma_p = sigma_e/n") + 
  ggtitle("Relationship between error-variances and ICC") + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  geom_vline(xintercept = 1, color = "red") + 
  annotate("text", x = 1.7, y = 0.1, label = "n=1", color = "red")

# Hence, the variance sigma_p should be larger than the error variance in comparison!
df_ <- df %>% filter( between(ICC, 0.60, 0.90) )
range(df_$factor_n) # 0.12 0.66
1/range(df_$factor_n) # sigma_p should be 1.5 to 8.3 times larger than sigma_e for ICC 60-90%

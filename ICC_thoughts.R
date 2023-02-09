# ICC, some thoughts:

n <- 1:20
sigma_e <- 1
sigma_p <- sigma_e/n

ICC <- sigma_p/(sigma_p + sigma_e)

df <- data.frame(sigma_p = sigma_p, proportion = n, ICC = ICC)
df %>% ggplot(aes(x = proportion, y = ICC)) + 
  geom_line() + xlab("sigma_p*n = sigma_e")

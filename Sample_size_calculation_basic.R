# Sample size calculations # 
# Some thoughts and basic examples # 

# Some helpful links/books:
# https://med.und.edu/research/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module.pdf and maybe
# https://med.und.edu/research/daccota/_files/pdfs/berdc_resource_pdfs/sample_size_r_module_glmm2.pdf

# Chapter 10 of: http://www.cs.uni.edu/~jacobson/4772/week11/R_in_Action.pdf
# 4.4 of: https://link.springer.com/book/10.1007/978-3-319-19425-7





# 1) Mini-Example from QM1 Script, p. 176---------------------------------------
power.t.test(delta = 0.5, sd = 1, power = 0.8, type = "one.sample", alternative = "one.sided")



# 2) Real-life-Example: two-armed RCT: -----------------------------------------

# __Determine alpha and power----
alpha <- 0.05
power <- 0.8

# __Determine sample size----
# ES rate - Jennings 2015
(effect_size <- (14.1-17.5)/12.5) # approx equal variances
# C-reactive protein - Niedermann 2013
(effect_size <- (4.95-6.27)/7.8) # approx equal variances

# ES rate - Jennings 2015
# https://thenewstatistics.com/itns/2021/06/17/which-standardised-effect-size-measure-is-best-when-variances-are-unequal/
(effect_size <- effect_size_d_star <- (31.1-17.5)/sqrt(16.4^2/2 + 10.5^2/2)) # try Cohen's d_star
# C-reactive proteine - Jennings 2015
(effect_size <- effect_size_d_star <- (4.95-7.14)/sqrt(4.86^2/2 + 8.21^2/2)) # try Cohen's d_star

if(effect_size > 0){ 
  alternative <- "greater"
} else if (effect_size < 0) {
  alternative <- "less"
}
# __Determine power----
pwr.t.test(d = effect_size, 
           n = NULL, 
           sig.level = alpha, 
           power = power, 
           type = "two.sample", 
           alternative = alternative) # One-side should work here since we are interested in a decrease in inflammation

# __Try range of effect sizes to illustrate influence:----
effect_sizes <- seq(0.05, 0.9, by = 0.05)
sample_sizes <- rep(NA, length(effect_sizes))

for(i in 1:length(effect_sizes)){
  test <- pwr.t.test(d = effect_sizes[i], 
                     n = NULL, 
                     sig.level = alpha, 
                     power = power, 
                     type = "two.sample", 
                     alternative = "greater") 
  sample_sizes[i] <- test$n
}

df <- data.frame(effect_sizes = effect_sizes, sample_size = sample_sizes)
df %>% filter(effect_sizes >= 0.2) %>%
  ggplot(aes(x = effect_sizes, y = sample_size)) + 
  geom_line() + 
  geom_point(x = 0.20, y = 309.80647, color = "blue", size = 5) + # highlighting typical effect sizes
  annotate("text", x = 0.25, y = 309.80647, label = "(0.20, 310)", color = "blue") +
  geom_point(x = 0.50, y = 50.15080, color = "blue", size = 5) +
  annotate("text", x = 0.55, y = 50.15080, label = "(0.50, 50)", color = "blue") +
  geom_point(x = 0.80, y = 20, color = "blue", size = 5) +
  annotate("text", x = 0.8, y = 40, label = "(0.80, 20)", color = "blue") +
  ggtitle("Effect sizes and sample sizes (per group) for power = 0.8 and alpha = 0.05") + 
  xlab("Effect size") + ylab("Sample size per group") + 
  theme_dark() + 
  theme(plot.title = element_text(hjust = 0.5))

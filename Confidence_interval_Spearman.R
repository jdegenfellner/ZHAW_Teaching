# Confidence intervals Spearman

library(pacman)
p_load(DescTools)

set.seed(332)
x <- sample(1:7, 25, replace = TRUE)
y <- round(1.2*x + rnorm(25, sd = 2.5))
plot(x,y)

cor(x,y, method = "spearman")
SpearmanRho(x,y, conf.level = 0.95)
# rho    lwr.ci    upr.ci 
# 0.7761999 0.5496456 0.8964034 
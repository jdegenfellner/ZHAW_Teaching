
# Slide 33, LM2.pdf
set.seed(22)
nage <- 3 ## Anzahl Kategorien Altersgruppe
ntherapy <- 2 ## Anzahl Kategorien Therapieart
n <- 200 ## Totale Sample Size
age <- factor(sample(c("child", "young", "old"), size = n, replace = TRUE, prob = c(1, 2, 3)), levels = c("child", "young", "old")) 
therapy <- factor(sample(c("Ctrl", "Trt"), size = n, replace = TRUE))
beta1 <- 40 ## Referenz
betaAge <- c(10, 20) ## betaAge2 und betaAge3
betaTr <- c(10) ## betaTreat
alphabeta <- c(-0, 0) ## youngTrt und oltTrt, Interaktionseffekte=0
parameter <- c(beta1, betaAge, betaTr, alphabeta) ## Wahrer Parametervektor
sigma <- 12 ## Noise SD
epsilon <- rnorm(n, 0, sigma) ## Fehler
X <- model.matrix(~age * therapy) ## Design-Matrix
response <- as.numeric(X %*% parameter + epsilon) ## Y=Xbeta+epsilon, Daten ziehen aus Modell
d.cat2 <- data.frame(response, age, therapy)

str(d.cat2)

mod <- lm(response ~ therapy*age, data = d.cat2) # "*" test main AND interaction effects, ":" is only the interaction effect
# from help: The * operator denotes factor crossing: a*b is interpreted as a + b + a:b.
summary(mod)
tbl_regression(mod, intercept = TRUE)

# What about R^2?
library(semEff)
R2(mod, adj.type = "olkin-pratt", pred = FALSE)[2] # Unbiased estimator
summary(mod)$r.squared
summary(mod)$adj.r.squared

tbl_regression(mod)
coef(mod)
predict(mod, newdata = data.frame(therapy = "Trt", age = "old")) # 69.34672
# check manually (just plug in):
34.8891800 + 6.3137427 + 26.8931452 + 1.2506546 # 69.34672


# What happens here?
mod_int_only <- lm(response ~ therapy:age, data = d.cat2)
summary(mod_int_only)
tbl_regression(mod_int_only, intercept = TRUE)
predict(mod_int_only, newdata = data.frame(therapy = "Ctrl", age = "child")) # 34.88918 
# manually:
69.347 - 34.458 # 34.889, identical
# It seems that just a normal regression is estimated in the respective sub-groups
# Let's verify this:
d.cat2_sub1 <- d.cat2 %>% filter(therapy == "Ctrl", age == "child")
mod_sub1 <- lm(response ~ 1, data = d.cat2_sub1) # Use only intercept
summary(mod_sub1) # Estimate = 34.889, This is just the mean in the subgroup!

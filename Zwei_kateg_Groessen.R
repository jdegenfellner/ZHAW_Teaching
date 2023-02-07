
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
#summary(mod)
tbl_regression(mod)
coef(mod)
predict(mod, newdata = data.frame(therapy = "Trt", age = "old")) # 69.34672
# check manually:





#mod_int_only <- lm(response ~ therapy:age, data = d.cat2) # "*" test main AND interaction effects, ":" is only the interaction effect
#summary(mod_int_only)
#tbl_regression(mod_int_only)

str(d.cat2)


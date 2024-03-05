# Eine kategorielle und eine kontin. Eingangsgroesse

library(pacman)
p_load(tidyverse,
       flextable,
       gtsummary,
       performance,
       ggpmisc)

# Create data
set.seed(10)
n.groups <- 2
n.sample <- 50
n <- n.groups * n.sample ##sample size
ind <- rep(1:n.groups, each = n.sample) ##Indicator for group 
group <- factor(ind, labels = c("A", "B"))
height <- rnorm(n, mean = 165, sd = 11.4)
covariates <- data.frame(group, height)
Xeffects <- model.matrix(~group * height)
Xmeans <- model.matrix(~group * height - height - 1) 
sigma <- 2
betaM <- c(muA <- -36.475, muB <- -45.5, slopeA <- 0.615, slopeB <- 0.7) ##Means-Param. 
betaE <- c(muA, muB - muA, slopeA, slopeB - slopeA) ##Effekt-Parm
lin.pred <- Xeffects %*% betaE
lin.pred2 <- Xmeans %*% betaM
# all.equal(lin.pred,lin.pred2) ## ist dasselbe
eps <- rnorm(n = n, mean = 0, sd = sigma) ## add noise 
weight <- lin.pred + eps ## Zielgrösse
d.catcont <- data.frame(group, height, weight)

d.catcont %>% ggplot(aes(x = height, y = weight, colour = group)) + 
  geom_point() + 
  ggtitle("Weight and height scatterplot for two groups A/B") +  
  theme(plot.title = element_text(hjust = 0.5)) + # Center title
  geom_smooth(aes(group = group), method = "lm") # add regression lines

# How about adding regression parameters?
d.catcont$heightCent <- scale(d.catcont$height, scale = FALSE)
d.catcont$heightCent <- as.vector(d.catcont$heightCent) # necessary since the previous line creates a matrix which causes problems with predict() later on
d.catcont %>% 
  ggplot(aes(x = heightCent, y = weight, colour = group)) + 
  geom_point() + 
  geom_smooth(aes(group = group), method = "lm") + # Add regression lines
  stat_poly_eq(aes(label = paste(after_stat(eq.label), 
                                 after_stat(rr.label), sep = "~~~")), 
               formula = y ~ x, 
               parse = TRUE, size = 3) + # Add regression equations
  ggtitle("Weight and height scatterplot for two groups A/B") +  
  theme(plot.title = element_text(hjust = 0.5)) # Center title


mod <- lm(weight ~ group*heightCent, data = d.catcont)
summary(mod)

# We always check the model fit/assumptions:
check_model(mod)
qqnorm(residuals(mod))
qqline(residuals(mod)) # does not look normal
check_normality(mod) # further hint of non-normality
confint(mod)
tbl_regression(mod, intercept = TRUE)

# "Predict":
predict(mod, newdata = data.frame(group = "B", heightCent = 2.3)) # 70.09261
# manually:
64.07758 + 4.41906 + 0.60936*2.3 + 0.08454*2.3 # 70.09261, identical
#(0.60936+ 0.08454)*2.3

# Slide 71, Steigung Gruppe B:
library(Publish)
mod %>% publish()
summary(mod)
# manuell, Steigung Gruppe B
0.60936156 + 0.08454184 # 0.6939034


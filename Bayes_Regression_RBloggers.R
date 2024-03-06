# Bayes regression

# https://www.r-bloggers.com/2020/04/bayesian-linear-regression/

suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))
data("BostonHousing")
str(BostonHousing)
library(tidyverse)

bost <- BostonHousing[,c("medv","age","dis","chas")]
summary(bost)

?BostonHousing # for details about the data!
# medv	median value of owner-occupied homes in USD 1000's
# age	proportion of owner-occupied units built prior to 1940
# dis	weighted distances to five Boston employment centres
# chas	Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)


# 1) Classical linear regression model----
model_freq <- lm(medv ~., data = bost)
summary(model_freq)
confint(model_freq) # recht aehnlich wie Bayes

# 2) Bayesian regression----
model_bayes<- stan_glm(medv ~., data = bost, seed = 111)

print(model_bayes, digits = 3)
prior_summary(model_bayes)

# Predict individual observation
df <- data.frame(medv = 76.5, age = 34, dis = 5.03, chas = 1)
df$chas <- as.factor(df$chas)

predict(model_bayes, newdata = df) # 34.17305 
predict(model_freq, newdata = df) # 34.15939 


# The Median estimate is the median computed from the MCMC simulation, 
# and MAD_SD is the median absolute deviation computed from the same 
# simulation. To well understand how getting these outputs let’s plot the 
# MCMC simulation of each predictor using bayesplot

mcmc_dens(model_bayes, pars = c("age"))+ # von der posterior
  vline_at(-0.143, col="red")

# As you see the point estimate of age falls on the median of this 
# distribution (red line). The same thing is true for dis and shas predictors.

mcmc_dens(model_bayes, pars=c("chas1"))+
  vline_at(7.496, col="red")

mcmc_dens(model_bayes, pars=c("dis"))+
  vline_at(-0.244, col="red") + 
  ggtitle("posterior distribution of dis")

# Now, how can we evaluate the model parameters? The answer is by analyzing 
# the posteriors using some specific statistics. To get the full statistics 
# provided by bayestestR package, we make use of the function describe_posterior

library(flextable)
flextable(describe_posterior(model_bayes, 
                             ci_method = "HDI", # Highest Density Interval (HDI), All points within this interval have a higher probability density than points outside the interval. The HDI can be used in the context of uncertainty characterisation of posterior distributions as Credible Interval (CI).
                             rope_range = "default", # = sd(bost$medv)*0.1 ... If "default", the bounds are set to x +- 0.1*SD(response)
                             diagnostic = NULL))
hdi(model_bayes, ci = 0.99)
plot(p_direction(model_bayes))
pd_to_p(0.81925) # nicht ganz exakt ... 


# Posterior Predictive Check:
ppc_data <- posterior_predict(model_bayes) # Generate posterior predictive data
ppc_dens_overlay(y = bost$medv, yrep = ppc_data) # takes a while!  ... could lool better, I guess
# The posterior_predict function is used to generate new data based on 
# the model, and then ppc_dens_overlay is used to compare the density of 
# the observed data and the simulated data.

# Further visualizations
# https://easystats.github.io/see/articles/bayestestR.html
result <- estimate_density(model_bayes, select = c("dis","chas"))
plot(result)
#plot(result, stack = FALSE, priors = TRUE)

result <- p_direction(model_bayes)
plot(result)

result <- p_direction(model_bayes)
plot(result, priors = TRUE)

result <- p_significance(model_bayes, threshold = "default")
result
plot(result)
# probability of Practical Significance (ps), which can be conceptualized 
# as a unidirectional equivalence test. It returns the probability that 
# effect is above a given threshold corresponding to a negligible effect in 
# the median's direction. Mathematically, it is defined as the proportion of 
# the posterior distribution of the median sign above the threshold.






# Aufgabe:-----

# Versuche mit anderen priors zu arbeiten (z.B. mit

# a) uninformative (prior = NULL) oder b) 
model_bayes_uninf_prior <- stan_glm(medv ~., data = bost, 
                                    prior = NULL, 
                                    prior_intercept = NULL)
#summary(model_bayes_uninf_prior)
coef(model_bayes)
#summary(model_bayes)
coef(model_bayes_uninf_prior)

# b) schiefen Verteilungen (z.B. Beta-Verteilung mit alpha = 2, beta = 5)
model_bayes_skewed_prior <- stan_glm(medv ~., data = bost, 
                                    prior = list(beta(2,5),beta(2,5),beta(2,5)), # ERROR...TODO
                                    prior_intercept = NULL)
model_bayes_skewed_prior <- stan_glm(
  medv ~ ., 
  data = bost, 
  prior = student_t(3, 0, 10), 
  prior_intercept = NULL
)

coef(model_bayes_skewed_prior)
coef(model_bayes)


# und prüfe inwiefern sich die Ergebnisse
# von der klassischen Regression unterscheiden.

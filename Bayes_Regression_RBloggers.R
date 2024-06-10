# Bayes regression

# see: https://www.r-bloggers.com/2020/04/bayesian-linear-regression/
# from: https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/Bayes_Regression_RBloggers.R

# see also Kruschke: https://jkkweb.sitehost.iu.edu/BMLR/index.html

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pacman)
p_load(mlbench,
       rstanarm,
       bayestestR,
       bayesplot,
       insight,
       broom,
       tidyverse,
       flextable,
       tictoc,
       performance)

data("BostonHousing")
str(BostonHousing)

bost <- BostonHousing[,c("medv","age","dis","chas")]
summary(bost)
# more data sets: https://archive.ics.uci.edu/

?BostonHousing # for details about the data!

# medv:	median value of owner-occupied homes in USD 1000's
# age: proportion of owner-occupied units built prior to 1940
# dis: weighted distances to five Boston employment centres
# chas:	Charles River dummy variable (= 1 if tract bounds river; 0 otherwise)


# 1) Classical linear regression model----
model_freq <- lm(medv ~., data = bost)
summary(model_freq)
# ....check model assumptions before interpreting any coefficients....
confint(model_freq) # recht aehnlich wie Bayes



# 2) Bayesian regression----
tic()
model_bayes <- stan_glm(medv ~., data = bost, seed = 111) # one can also use the package brms
toc() # 0.584 sec elapsed

print(model_bayes, digits = 3)

# What priors where used for the parameters?----
prior_summary(model_bayes)

# Predict individual observation----
df <- data.frame(medv = 76.5, age = 34, dis = 5.03, chas = 1)
df$chas <- as.factor(df$chas)

predict(model_bayes, newdata = df) # 34.17305 
predict(model_freq, newdata = df) # 34.15939 
# -> very similar

# The Median estimate is the median computed from the MCMC simulation, 
# and MAD_SD is the median absolute deviation computed from the same 
# simulation. To well understand how getting these outputs let’s plot the 
# MCMC simulation of each predictor using bayesplot

coef(model_bayes) # also works in Bayes

mcmc_dens(model_bayes, pars = c("age"))+ # von der posterior
  vline_at(-0.143, col="red") + 
  ggtitle("Posterior of coefficent age") + 
  theme(plot.title = element_text(hjust = 0.5))

# As you see the point estimate of age falls on the median of this 
# distribution (red line). The same thing is true for dis and shas predictors.

mcmc_dens(model_bayes, pars=c("chas1"))+
  vline_at(7.496, col="red") + 
  ggtitle("Posterior of coefficent chas1") + 
  theme(plot.title = element_text(hjust = 0.5))

mcmc_dens(model_bayes, pars=c("dis"))+
  vline_at(-0.244, col="red") + 
  ggtitle("posterior distribution of dis")+ 
  theme(plot.title = element_text(hjust = 0.5))

# Now, how can we evaluate the model parameters? The answer is by analyzing 
# the posteriors using some specific statistics. To get the full statistics 
# provided by bayestestR package, we make use of the function describe_posterior

flextable(describe_posterior(model_bayes, 
                             ci_method = "HDI", # Highest Density Interval (HDI), All points within this interval have a higher probability density than points outside the interval. The HDI can be used in the context of uncertainty characterisation of posterior distributions as Credible Interval (CI).
                             rope_range = "default", # = sd(bost$medv)*0.1 ... If "default", the bounds are set to x +- 0.1*SD(response)
                             diagnostic = NULL))
bayestestR::hdi(model_bayes, ci = 0.99)
plot(p_direction(model_bayes))
#pd_to_p(0.81925) # nicht ganz exakt ... 


# Posterior Predictive Check:----
ppc_data <- posterior_predict(model_bayes) # Generate posterior predictive data
#tic()
#ppc_dens_overlay(y = bost$medv, yrep = ppc_data) # there seem to be structural problems
#toc() # 13.224 sec elapsed
# much FASTER:
tic()
check_predictions(model_bayes) # package performance
toc() # 0.293 sec elapsed
# -> does NOT look good

# The posterior_predict function is used to generate new data based on 
# the model, and then ppc_dens_overlay is used to compare the density of 
# the observed data and the simulated data.

p_direction(model_bayes)

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

# siehe auch: https://mc-stan.org/rstanarm/articles/priors.html

# a) uninformative (prior = NULL)----
tic()
model_bayes_uninf_prior <- stan_glm(medv ~., data = bost, 
                                    prior = NULL, 
                                    prior_intercept = NULL)
toc() # 0.651 sec elapsed
print(model_bayes_uninf_prior, digits = 3)
coef(model_bayes)
#summary(model_bayes)
coef(model_bayes_uninf_prior)

prior_summary(model_bayes_uninf_prior) # flat priors: This indicates that the prior for the intercept is flat, meaning every value is equally probable. This is often represented by a uniform distribution over a very large range, effectively placing no constraints on the intercept based on prior knowledge.


# TODO
# b) schiefen Verteilungen (z.B. Beta-Verteilung mit alpha = 2, beta = 5)----

# Modell mit schiefer Prior (Beta-Verteilung)
priors <- c(
  set_prior("gamma(2, 0.5)", class = "b"),
  set_prior("normal(0, 10)", class = "Intercept"),
  set_prior("cauchy(0, 2.5)", class = "sigma")
)
# Modell definieren und ausführen
tic()
model_bayes_skewed_prior <- brm(
  medv ~ ., 
  data = bost, 
  prior = priors,
  family = gaussian(),
  chains = 4,
  iter = 5000
)
toc() # 27.357 sec elapsed
# warnings "It appears as if you have specified a lower bounded prior on a parameter that has no natural lower bound."

summary(model_bayes_skewed_prior)

model_bayes_skewed_prior
coef(model_bayes)

# und prüfe inwiefern sich die Ergebnisse
# von der klassischen Regression unterscheiden.

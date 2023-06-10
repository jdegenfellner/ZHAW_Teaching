# Bayes Estimation for LMM from before

# in progress #

library(brms)
library(rstanarm)
library(rstan)
library(mlbench)
library(bayestestR)
library(bayesplot)
library(insight)
library(broom)
library(tidyverse)
#remotes::install_github("poisonalien/flatuicoloRs")
library(flatuicoloRs)


# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# READ----
d.lmm <- read.csv("./UEB_LMM_files/lmm1.csv", sep = ",", stringsAsFactors = TRUE) # adapt to your path
d.lmm$timeNum <- as.numeric(d.lmm$time)
str(d.lmm)

# previous model:
m.ri <- lmer(response ~ timeNum + (1|subject), data = d.lmm)
summary(m.ri)

# Estimate model-----
lmm_bayes <- stan_glmer(response ~ timeNum + (1 | subject), data = d.lmm, family = gaussian())
summary(lmm_bayes)

coef(lmm_bayes)
plot(lmm_bayes, pars = c("(Intercept)", "timeNum"))

# Plot density of POSTERIOR distributions (after applying MCMC)
mcmc_dens(lmm_bayes, pars = c("timeNum")) +
  vline_at(1.5223, col="red")

mcmc_dens(lmm_bayes, pars = c("(Intercept)")) +
  vline_at(20.2 , col="red")

point_estimate(lmm_bayes, centrality = "all") # centrality: The point-estimates (centrality indices) to compute. Character (vector) or list with one or more of these options: "median", "mean", "MAP" or "all".
# MAP = Maximum a posteriori estimation = Mode of the distribution
plot(point_estimate(lmm_bayes)) # https://easystats.github.io/see/articles/bayestestR.html
hdi(lmm_bayes, ci = c(0.5, 0.75, 0.89, 0.95))

plot(hdi(lmm_bayes, ci = c(0.5, 0.75, 0.89, 0.95)))

rope(lmm_bayes, ci = c(0.9, 0.95))
plot(rope(lmm_bayes, ci = c(0.9, 0.95)))

plot(rope(lmm_bayes, ci = c(0.9, 0.95)), rope_color = "red") +
  scale_fill_brewer(palette = "Greens", direction = -1)



# Posterior predictive check:-----
# a way to validate a Bayesian model. The idea is to generate new data from 
# the model using the posterior distributions of the parameters, and compare 
# these to the observed data.

# Description of pp_check():
# 1) For each sample from the posterior distribution of the model parameters, 
# the function generates a simulated dataset. The number of these datasets 
# equals the number of posterior samples.

# 2) The function then compares these simulated datasets to the actual 
# observed data. This can be done in many ways, and the default in 
# pp_check() is to compare the distributions of the observed and 
# simulated data. This is done using a graphical method, which in this 
# case is overlaying the distributions on the same set of axes to facilitate comparison.

# 3) The resulting graph shows the distribution of the observed data
# overlaid with the distribution of the simulated data. If the model 
# is a good fit, we would expect these two distributions to be similar.

#Differences between the observed and simulated distributions suggest that the model might not be capturing some aspects of the data. However, it's important to note that this doesn't necessarily mean that the model is "wrong", just that there are aspects of the data that it doesn't capture perfectly.
pp_check(lmm_bayes) # does not look bad


stan_diag(lmm_bayes) # Diagnostics of the MCMC algorithm....


# Model comparison in Bayes-----------
lmm_bayes2 <- stan_glmer(response ~ timeNum + (timeNum | subject), data = d.lmm, family = gaussian())
loo1 <- loo(lmm_bayes)
loo2 <- loo(lmm_bayes2)
loo_compare(loo1, loo2) # For models fit using MCMC, compute approximate
# leave-one-out cross-validation (LOO, LOOIC) or, less preferably, 
# the Widely Applicable Information Criterion (WAIC) using the loo package.

# Info: The Expected Log Predictive Density (ELPD) is a measure of the 
# expected log pointwise predictive density, which is used to compare 
# different models in terms of their predictive accuracy. The log 
# pointwise predictive density is the logarithm of the predictive 
# density of a new observation given the observed data.

# ELPD is a key part of the Widely Applicable Information Criterion (WAIC) 
# and Leave-One-Out Cross-Validation (LOO-CV), two popular methods of model 
# comparison and selection. In both of these methods, a higher ELPD value
# indicates a model that is expected to be better at prediction.

# Baysian hypothesis testing!-------
hypothesis(lmm_bayes, "timeNum = 0") # uses package brms
# it appears that it has tested the null hypothesis that timeNum = 0 and 
# found strong evidence against it. The estimate of timeNum is 1.52, with 
# a 95% credible interval from 0.54 to 2.53, meaning that there is a 95% 
# probability that the true value of timeNum is within this interval, given 
# the observed data and the prior distribution. Because this interval does 
# not include 0, the data provides strong evidence against the null hypothesis 
# that timeNum = 0.

posterior_interval(lmm_bayes)




library(pacman)
p_load(report, Bolstad, BayesianFirstAid)
#devtools::install_github("rasmusab/bayesian_first_aid") # if not installed

# Ex 1.:
bayes.t.test(1:10, y = c(7:20)) 

# Ex 2.:
?sleep
str(sleep)
bayes.t.test(extra ~ group, data = sleep)


# Based on Kruschke, 2012:
# https://www.rdocumentation.org/packages/BayesianFirstAid/versions/0.1/topics/bayes.t.test


# Save the return value in order to inspect the model result further.
fit <- bayes.t.test(extra ~ group, data = sleep)
summary(fit)
plot(fit)
diagnostics(fit)


#report(sessionInfo())

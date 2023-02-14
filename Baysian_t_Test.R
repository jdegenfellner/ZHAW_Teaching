library(report)
library(Bolstad)
#devtools::install_github("rasmusab/bayesian_first_aid")

bayes.t.test(1:10, y = c(7:20)) 
bayes.t.test(extra ~ group, data = sleep)


library(BayesianFirstAid)
# Based on Kruschke, 2012:
# https://www.rdocumentation.org/packages/BayesianFirstAid/versions/0.1/topics/bayes.t.test


# Save the return value in order to inspect the model result further.
fit <- bayes.t.test(extra ~ group, data = sleep)
summary(fit)
plot(fit)
diagnostics(fit)


#report(sessionInfo())

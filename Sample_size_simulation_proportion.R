# Notes-----

# Primary endpoint: Proportion of patients with toxicity GU>=2 after 2 years.

# FASTR: https://pubmed.ncbi.nlm.nih.gov/25936597/
# Meta-Analysis: https://www.redjournal.org/article/S0360-3016(23)08006-9/abstract

# First, we need to note that for the FASTR-study a sample size of 60 where
# calculated (not stated how), but only 15 persons were recruited. With a sample
# size that small, variability is high, and the estimate is probably inaccurate.

# Presumably, the confidence intervals in Fig. 3. (Forest plot) are Wald-type
# intervals: p_bar + c(-1,1)*qnorm(0.975)*s/sqrt(n)
# Example: study Bauman 2015:
6/15 # 0.4 (5 Grade 2, 1 Grade 3)
s <- sqrt(0.4*0.6)
0.4 + c(-1,1)*qnorm(0.975)*s/sqrt(15) # approximately correct
binom.test(6,15) # 0.1633643 0.6771302
# It seems, Bauman used exact confidence intervals (Clopper and Pearson (1934))


# Essentially, one needs to estimate a proportion in a population.



# Approach - Simulation-------
library(pacman)
p_load(tidyverse)

p_true <- 0.3 # true but unknown proportion of patients with GU>=2 after 2 years (later)
n_people <- 40 # study size
n_sim <- 100000
res <- rbinom(n_sim, n_people, prob = p_true)/n_people # Parameter estimation
# simulated proportions of patients GU>=2, given the true but unknown parameter

hist(res)
mean(res) # ~n_people*p_true (according to law of large numbers)
quantile(res, probs = c(0.05,0.95)) 

# In what proportion of cases does the estimated proportion deviate by 0.12 or less?-------
sum(abs(res-p_true) <= 0.12)/n_sim # = simulated power to estimate proportion within preset difference of e.g.,0.12

power_simulate <- function(p_true = 0.3, # True but unknown proportion of GU>=2 after 2 years
                           n_people = 40, # Number of participants (after dropout)
                           n_sim = 10000, # Number of simulations
                           abs_perc_diff = 0.12 # Absolute difference to true but unknown proportion.
                           ){
  # returns simulated proportions of GU>=2, given the true but unknown parameter
  res <- rbinom(n_sim, n_people, prob = p_true)/n_people 
  return(sum( abs(res-p_true) <= abs_perc_diff )/n_sim)
}

# Example----
power_simulate(p_true = 0.3,
               n_people = 38,
               n_sim = 10000,
               abs_perc_diff = 0.12)
# ~ 88%


# Ceteris paribus changes (=sensitivity analysis)----

# For _1) to _3), we set default values:
dropout <- 0.05
n_people <- 40 # study size
(n_people_inkl_dropout <- n_people*(1-dropout)) # after dropout
p_true_meta <- 0.3 # let's take this value from the meta-analysis
abs_perc_diff_meta <- 0.12 # just an example


# _1) Change p_true----
p_true_seq <- seq(0.01, 0.99, by = 0.01)
power_values_p_seq <- unlist(lapply(p_true_seq, function(p) 
  power_simulate(p_true = p, n_people = n_people_inkl_dropout, n_sim = 10000)))

data.frame(p_true_seq,
           power_values_p_seq) %>%
  ggplot(aes(x = p_true_seq*100, y = power_values_p_seq)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("True but unknown proportion (in%) of GU>=2 after 2 years") + 
  ylab("(simulated) Power") + 
  geom_hline(yintercept = 0.8, color="red") + 
  geom_vline(xintercept = 0.3*100, color="red") + 
  ggtitle("Sensitivity analysis for true (but unknown) proportion of GU>=2 (after 2 years)")+ 
  theme(plot.title = element_text(hjust = 0.5))
# power is highest on the margins since variance is smaller and difference to 
# true value is defined absolutely (default 12%).
# In this case, one is uniformly above (simulated) Power = 0.8

# _2) Change n_people----
n_people_seq <- seq(20, 80, by = 1)
power_values_n_people_seq <- unlist(lapply(n_people_seq, function(p) 
  power_simulate(p_true = p_true_meta, n_people = p, n_sim = 10000)))

data.frame(n_people_seq = n_people_seq,
           power_values_n_people_seq) %>%
  ggplot(aes(x = n_people_seq, y = power_values_n_people_seq)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Number of participants (after dropout)") + 
  ylab("(simulated) Power") + 
  geom_vline(xintercept = n_people_inkl_dropout, color="red") + 
  geom_hline(yintercept = 0.8, color="red") +
  ggtitle("Sensitivity analysis for number of participants") + 
  theme(plot.title = element_text(hjust = 0.5))
# The higher n_people, the higher the power

# _3) Change abs_perc_diff----
abs_perc_diff <- seq(0.01, 0.2, by = 0.01) # more than 0.2 makes no sense (asymptote)
power_values_abs_perc_diff_seq <- unlist(lapply(abs_perc_diff, function(p) 
  power_simulate(p_true = p_true_meta, n_people = n_people_inkl_dropout, 
                 n_sim = 10000, abs_perc_diff = p)))

data.frame(abs_perc_diff = abs_perc_diff,
           power_values_abs_perc_diff_seq) %>%
  ggplot(aes(x = abs_perc_diff*100, y = power_values_abs_perc_diff_seq)) + 
  geom_point() + 
  geom_smooth() + 
  xlab("Absolute percentage difference to true but unknown proportion of GU>=2 after 2 years") + 
  ylab("(simulated) Power") + 
  geom_hline(yintercept = 0.8, color="red") + 
  geom_vline(xintercept = abs_perc_diff_meta*100, color="red") + 
  ggtitle("Sensitivity analysis for abs. percentage difference to true proportion of GU>=2")+ 
  theme(plot.title = element_text(hjust = 0.5))
# The higher the absolute percentage difference allowed, the higher the power.
# One also sees that this goes down rather fast with lower absolute difference.
# E.g., with 5% difference, one would only have 50% Power and sample size
# would need to be increased.

# Summary------

# Assuming 5% dropout, a true proportion of GU>=2 after 2 years of 0.3
# and a percentage difference of 12% to 0.3, the simulated power to estimate 
# the proportion adequately is above 80%.

# Sensitivity analysis shows that this result is robust.
# Also note, that the simulated curves _1) to _3) are highly stable with respect
# to repetition.


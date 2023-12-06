library(pacman)
p_load(tidyverse)

# From:
# https://github.com/jdegenfellner/ZHAW_Teaching/blob/main/t_Test.R

# Functions:
plot_t_density <- function(dof = 8, t_value = -2.683282){

  t_values <- seq(from = qt(0.001, dof), to = qt(0.999, dof), length.out = 100)
  densities <- dt(t_values, dof)
  
  ggplot(data.frame(t_values, densities), aes(x = t_values, y = densities)) +
    geom_line() +
    geom_vline(xintercept = t_value, color = "red", linetype = "dashed") +
    ggtitle(paste("t-Distribution with df =", dof, 
                  "and vertical line at t_value =", round(t_value,2))) +
    xlab("t value") + ylab("Density") + 
    theme(plot.title = element_text(hjust = 0.5, size = 8))
}

# Let's start with the

# 1) One Sample t-Test----
# see also: https://de.wikipedia.org/wiki/Einstichproben-t-Test
mu0 <- 14
X <- c(10, 10, 12, 10, 14, 14, 12, 16, 10)

# _Manually----
(mean_X <- mean(X))
(n <- length(X))
(s2 <- var(X))
(s <- sd(X))
(se <- s / sqrt(n)) # se(\bar{X}), standard error of the arithmetic mean
(t_value <- (mean_X - mu0) / se)

# Under H_0, the test-statistic is t-distributed with 
# n-1 degrees of freedom (dof)
plot_t_density(dof = n - 1, t_value = t_value)
qt(0.025, n-1)

# _Direct t-test----
t.test(X, mu = mu0, alternative = "two.sided")
# How do I get the p-value?
pt(t_value, df = n - 1)*2 # because two-sided alternative!
qt(0.025, df = n - 1) # former: in tables...
# How about one-sided?
t.test(X, mu = mu0, alternative = "less")
pt(t_value, df = n - 1) # check
qt(0.05, df = n -1) # cut off


# _Confidence Interval----
(ci <- mean_X + c(-1, 1) * qt(.975, n - 1) * se)

# 2) Two-sample t-Test for two independent samples----
attach(sleep)
head(sleep)
str(sleep)
ggplot(sleep, aes(x = group, y = extra)) +
  geom_boxplot() +
  ggtitle("Extra Sleep in Pharmacology Groups") +
  xlab("Group") + ylab("Extra Sleep (hours)") + 
  theme(plot.title = element_text(hjust = 0.5, size = 10))

sleep %>% group_by(group) %>%
  summarise(group_mean = mean(extra))

# Two-sample t-tests
(test1 <- t.test(extra ~ group, var.equal = FALSE))
(test2 <- t.test(extra ~ group, var.equal = TRUE))

# ANOVA (see QM2)
test3 <- aov(extra ~ group, data = sleep) # same as with var.equal=TRUE
summary(test3)

# 3) Paired Data----
set.seed(3)
X <- rnorm(20, 4, 2)
Y <- rnorm(20, 7, 2)
df <- data.frame(X, Y)

# Plot
matplot(rbind(X, Y), type = "b", xaxt = "n", 
        xlab = "Time", ylab = "Outcome") # xact = x axis type, "n" = none; type = "b" Points and lines plot
axis(1, at = c(1, 2))


# _Paired t-test----
(test4 <- t.test(X, Y, paired = TRUE))
(test5 <- t.test(Y - X)) # mu = 0, alternative = "two.sided"

# Power of the t-test----
# _Example----
delta = 1.5 - 1# Cohen's d; effect size (=standardized difference)
power.t.test(n = 20, 
             delta = 0.5, 
             sd = 1,
             type = "one.sample",
             alternative = "one.sided")

# _a function of n:----
ns <- 3:100
powers <- c()
for (i in ns) {
  test <- power.t.test(n = i, 
                       delta = 0.5, 
                       sd = 1,
                       type = "one.sample",
                       alternative = "one.sided")
  powers <- append(powers, test$power)
}
df <- data.frame(power = powers, n = ns)
df %>% ggplot(aes(x = n, y = power)) + 
  geom_line() + 
  xlab("n")

# _a function of delta:----
deltas <- seq(from = 0, to = 1.5, by = 0.05)
powers <- c()
for (i in deltas) {
  test <- power.t.test(n = 20, 
                       delta = i, 
                       sd = 1,
                       type = "one.sample",
                       alternative = "one.sided")
  powers <- append(powers, test$power)
}
df <- data.frame(power = powers, delta = deltas)
df %>% ggplot(aes(x = delta, y = power)) + 
  geom_line() + 
  xlab("effect size (Cohen's d)")

# _a function of sd:----
sds <- seq(from = 0.1, to = 3, by = 0.05)
powers <- c()
for (i in sds) {
  test <- power.t.test(n = 20, 
                       delta = 0.5, 
                       sd = i,
                       type = "one.sample",
                       alternative = "one.sided")
  powers <- append(powers, test$power)
}
df <- data.frame(power = powers, sd = sds)
df %>% ggplot(aes(x = sd, y = power)) + 
  geom_line() + 
  xlab("sd")

# TODO (3 over 2 = ) 3 surface plots


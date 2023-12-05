library(pacman)
p_load(tidyverse)


# Hypothesis Testing with One Sample
# see also: https://de.wikipedia.org/wiki/Einstichproben-t-Test
mu0 <- 14
X <- c(10, 10, 12, 10, 14, 14, 12, 16, 10)

(mean_X <- mean(X))
(n <- length(X))
(s2 <- var(X))
(s <- sd(X))
(se <- s / sqrt(n))
(t_value <- (mean_X - mu0) / se)

# Under H_0, the test-statistik is t-distributed with n-1 degrees of freedom (dof)


# Direct t-test
t.test(X, mu = mu0)

# Confidence Interval
ci <- mean_X + c(-1, 1) * qt(.975, n - 1) * se
cat("Confidence Interval:", ci, "\n")

# Two Independent Samples
# Assuming 'sleep' dataset is available
attach(sleep)
plot(extra ~ group, data = sleep)
aggregate(extra, by = list(group), FUN = "mean")

# Two-sample t-tests
test1 <- t.test(extra ~ group, var.equal = FALSE)
test2 <- t.test(extra ~ group, var.equal = TRUE)
cat("Two Sample t-tests (unequal and equal variances):\n")
print(test1)
print(test2)

# ANOVA
test3 <- aov(extra ~ group, data = sleep)
summary(test3)

# Paired Data
set.seed(3)
X <- rnorm(20, 4, 2)
Y <- rnorm(20, 7, 2)
df <- data.frame(X, Y)

# Plot
matplot(rbind(X, Y), type = "b", xaxt = "n", xlab = "Time", ylab = "Outcome")
axis(1, at = c(1, 2))

# Paired t-test
test4 <- t.test(X, Y, paired = TRUE)
test5 <- t.test(Y - X)
cat("Paired t-tests:\n")
print(test4)
print(test5)

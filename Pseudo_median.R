# Pseudomedian.R

# in progress # 

# see also:
# https://stats.stackexchange.com/questions/335892/what-is-pseudomedian-in-r-function-wilcox-test
# https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/wilcox.test.R

# Example----
#x <- rnorm(4000)
#y <- rnorm(4000, mean = 0.5) # ...maybe choose skewed data

x <- c(13.22, 6.81, 10.22, 14.03, 8.04, 10.16, 9.43, 13.07, 13.63, 5.05, 11.63)
y <- c(15.44, 6.69, 11.89, 16.25, 9.27, 10.74, 10.67, 13.52, 14.13, 7.21, 14.79)

res <- wilcox.test(x, y, paired = TRUE, conf.int = TRUE)
#wilcox.test(x - y, conf.int = TRUE)
res$estimate
# compare to:
median(x - y) # should be very similar if symmetric

# Reproduce manually: ----
# In wilcox.test()-code (we should be in the one-sample case for x-y):
# see: https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/wilcox.test.R
diffs <- outer(x-y, x-y, "+")
diffs <- sort(diffs[!lower.tri(diffs)]) / 2
# ESTIMATE = 
c("(pseudo)median" = median(diffs))

# small deviations...?

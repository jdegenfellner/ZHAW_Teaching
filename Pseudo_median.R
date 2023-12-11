# Pseudomedian.R

# in progress # 

# see also:
# https://stats.stackexchange.com/questions/335892/what-is-pseudomedian-in-r-function-wilcox-test
# https://github.com/SurajGupta/r-source/blob/master/src/library/stats/R/wilcox.test.R

# Example
x <- rnorm(4000)
y <- rnorm(4000, mean = 0.5)
res <- wilcox.test(x, y, paired = TRUE, conf.int = TRUE)
#wilcox.test(x - y, conf.int = TRUE)
res$estimate

# Reproduce?:
# Hodges-Lehmann estimator for paired data
diffs <- x - y
average_diffs <- (outer(diffs, diffs, "+") / 2)[lower.tri(diffs)]
median(average_diffs) # close
median(average_diffs) - res$estimate

# in wilcox.test()-code (we should be in the one-sample case for x-y): 
diffs <- outer(x-y, x-y, "+")
diffs <- sort(diffs[!lower.tri(diffs)]) / 2
# ESTIMATE = 
c("(pseudo)median" = median(diffs))

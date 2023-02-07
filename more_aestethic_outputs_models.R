library(report)

mod <- lm(Fertility ~ ., data = swiss)
report(mod)
report_performance(mod)
report_parameters(mod)

report(sessionInfo())


library(performance)
check_model(mod)

# in progress

library(flextable) # more beautiful tables
library(gtsummary)

tbl_regression(mod) # create nice table for output

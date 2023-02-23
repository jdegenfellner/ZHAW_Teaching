library(report)

mod <- lm(Fertility ~ ., data = swiss) # Explain Fertility by ALL other variables in the data set
report(mod)
report_performance(mod)
report_parameters(mod)

report(sessionInfo())


library(performance)
check_model(mod)

# in progress

library(flextable) # more beautiful tables
library(gtsummary) # show regression results nicely

tbl_regression(mod) # create nice table for output
anova(mod)
tbl_regression(anova(mod))

library(rstatix)
flextable(anova_summary(anova(mod)))

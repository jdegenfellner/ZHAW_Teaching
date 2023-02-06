library(report)

lm(Fertility ~ ., data = swiss)
report(mod)
report_performance(mod)
report_parameters(mod)

report(sessionInfo())


library(performance)
check_model(mod)



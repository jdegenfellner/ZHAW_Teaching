# some calculations regarding:
# https://www.sciencedirect.com/science/article/pii/S0004951414602282?via%3Dihub

# library("reshape2")
library(tidyverse)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

specificity <- 0.99
sensitivity <- 0.95
prevalence <- seq(0.005, 0.15, by = 0.001)

TN <- (1-prevalence)*specificity
FP <- (1-prevalence)*(1-specificity)
FN <- prevalence*(1-sensitivity)
TP <- prevalence*sensitivity
MCE <- (FP+FN)/(TN+FP+FN+TP)
PPV <- TP/(TP+FP)
NPV <- TN/(FN+TN)
FPR <- FP/(TP+FP)
(LR_pos <- sensitivity/(1 - specificity))
(LR_neg <- (1-sensitivity)/specificity)






specificity <- 0.999
sensitivity <- 0.9652
tests_per_day <- 500000
prevalence <- seq(0.005, 0.15, by = 0.001)

#              tests_per_day
#               |         |
#               |         |
#       not_infected      infected
#       |         |       |      |
#       |         |       |      |
#       neg      pos     neg     pos
#       TN       FP      FN      TP      

TN <- tests_per_day*(1-prevalence)*specificity
FP <- tests_per_day*(1-prevalence)*(1-specificity)
FN <- tests_per_day*prevalence*(1-sensitivity)
TP <- tests_per_day*prevalence*sensitivity
MCE <- (FP+FN)/(TN+FP+FN+TP)
PPV <- TP/(TP+FP)
NPV <- TN/(FN+TN)
FPR <- FP/(TP+FP)

dat <- tibble(prevalence = prevalence, TN = TN, FP = FP, FN = FN, TP = TP)
dat_long <- melt(dat, id = "prevalence")  # convert to long format
ggplot(data=dat_long,
       aes(x=prevalence, y=value, colour=variable)) +
  geom_line()

dat1 <- tibble(prevalence = prevalence,PPV=PPV, FPR=FPR)
dat1_long <- melt(dat1, id="prevalence")  # convert to long format
ggplot(data=dat1_long,
       aes(x=prevalence, y=value, colour=variable)) +
  geom_line() + 
  ggtitle(paste0("Roche SARS-CoV-2 Rapid Antigen Test: Positive Predictive Value (PPV) and False Positive Rate (FPR), ", "Specificity= ", specificity, ", Sensitivity= ", sensitivity))+
  xlab("Disease Prevalence") + 
  ylab("Value")


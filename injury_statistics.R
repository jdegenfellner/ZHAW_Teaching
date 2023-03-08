library(emmeans)
library(data.table)
library(tidyverse)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

inj <- read.csv("./Data/serious-injury-outcome-indicators-2000-2020-CSV.csv",
                header = TRUE,
                sep = ",",
                dec = ".")

View(inj)
str(inj)

# Prepare-----------------------------------------------------------------------
inj$Period_posix <- as.POSIXct(strptime(inj$Period, "%Y-%m"))
# Problem: two different data formats in one variable!

unique(nchar(inj$Period)) # 4/7; two formats

# -> simplify to year only
inj$Period <- ifelse(nchar(inj$Period) == 7,substr(inj$Period,1,4), inj$Period)
unique(nchar(inj$Period)) # 4

sum(is.na(inj$Period))
inj$Period_posix <- as.POSIXct(strptime(inj$Period, "%Y"))

inj %>% ggplot(aes(x = Period_posix, y = Data_value, color = Severity)) + 
  geom_point()

summary(inj$Data_value)
boxplot(inj$Data_value)

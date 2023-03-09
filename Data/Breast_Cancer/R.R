# Breast Cancer data
# http://archive.ics.uci.edu/ml/datasets/Breast+Cancer

library(data.table)
library(tidyverse)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

brea <- read.csv("breast-cancer.data", header = FALSE, stringsAsFactors = TRUE)

str(brea)
View(brea)

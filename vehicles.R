library(data.table)
library(tidyverse)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

vehicles <- read.csv("./Data/Electric_Vehicle_Population_Data.csv")

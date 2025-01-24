library(data.table)
library(tidyverse)
library(DataExplorer)

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

vehicles <- read.csv("./Data/Electric_Vehicle_Population_Data.csv")
create_report(vehicles)
describe(vehicles)

pie(table(vehicles$Make))

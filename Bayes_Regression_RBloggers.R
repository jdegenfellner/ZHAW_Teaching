# Bayes regression

# https://www.r-bloggers.com/2020/04/bayesian-linear-regression/

suppressPackageStartupMessages(library(mlbench))
suppressPackageStartupMessages(library(rstanarm))
suppressPackageStartupMessages(library(bayestestR))
suppressPackageStartupMessages(library(bayesplot))
suppressPackageStartupMessages(library(insight))
suppressPackageStartupMessages(library(broom))
data("BostonHousing")
str(BostonHousing)

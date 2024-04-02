# Measurement in Medicine
# Calculating ICC with R and manually

#library(foreign)
library(tidyverse)
library(haven)
library(data.table)
library(lme4)
library(remotes)
library(devtools)
library(performance)
#install_remote("https://github.com/easystats/performance/blob/HEAD/R/icc.R", dependencies = TRUE)
library(psych)

# Note, There are also other packages with an ICC function, like psych, see lecture

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

set.seed(2332)

# 5.2 Example ------------------------------------------------------------------

df <- read_sav("./Data/chapter 5_assignment 1_4_long.sav")
df <- df %>% filter(patcode %in% sample(1:155, 50, replace = FALSE))
length(unique(df$patcode))
df <- as.data.table(df)

cor(df[df$Index1 == 1,]$trans1, df[df$Index1 == 2,]$trans1)
t.test(df[df$Index1 == 1,]$trans1, df[df$Index1 == 2,]$trans1)
plot(df[df$Index1 == 1,]$trans1, df[df$Index1 == 2,]$trans1)

# ICC
library(irr)
# https://www.r-bloggers.com/2021/06/intraclass-correlation-coefficient-in-r-quick-guide/

df_icc <- data.frame(R1 = df[df$Index1 == 1,]$trans1, R2 = df[df$Index1 == 2,]$trans1)
icc(df_icc, model = "oneway", type = "consistency", unit = "single")
# ICC(1) = 0.764

# ICC manually------------------------------------------------------------------
# see: 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6645485/pdf/pone.0219854.pdf.  
# p.9, Fig.1
# Note that the formula in Fig.1 does NOT use information from the estimated 
# LMM, but only uses information from the data matrix at hand.

# The package performance has a function icc which let's you choose the formula
# for the LMM.
# see also: https://easystats.github.io/performance/reference/icc.html for usage


# 1) Data transformation----
df1 <- df %>% select(-id,-trans2)
df1$Index1 <- factor(df1$Index1)
df1 <- df1 %>% spread(Index1, trans1)
colnames(df1) <- c("patcode", "Measurement1", "Measurement2")
df1 <- df1 %>% mutate(Mean = (Measurement1 + Measurement2)/2)
colnames(df1) <- c("patcode", "Measurement1", "Measurement2", "Mean_M1_M2")

# 2) Estimate correct model ----
model1 <- lmer(trans1 ~ (1|patcode), data = df) # this should be the correct 
                                                # model, since it's consistent with the other ICC-value

performance::icc(model1) # ICC: 0.764


# 3) Calculate sum of squares and so on (4 Letter abbreviations in paper)----
# k = 2 measurements
# MSBS = Mean square between subjects
# MSWS = Mean square within subjects
# SSBS = Sum of squares between subjects
# SSWS = Sum of squares within subjects
# SSBM = Sum of squares between measurements
# SSE = Sum of squares, error
# S_i = RowMeans... mean value for each subject
# M_j = Mean value for each measurement
# \bar{x} = Total mean value of all measurements

k <- 2
n <- dim(df1)[1]
S_i <- df1$Mean_M1_M2
M_j <- c( mean(df1$Measurement1), mean(df1$Measurement2) ) # colMeans()
x_bar <- mean( c(df1$Measurement1, df1$Measurement2) )
SSBS <- sum((S_i - x_bar)^2)*2 # in the formula there is no j in the sum argument?
MSBS <- SSBS/(n-1)
SSBM <- sum((M_j - x_bar)^2)*50 # in the formula there is no i in the sum argument?
df2 <- df1 %>% dplyr::select(Measurement1, Measurement2) %>%
  mutate(sum_intermediate1 = Measurement1 - S_i,
         sum_intermediate2 = Measurement1 - S_i) %>%
  mutate(sum_2 = sum_intermediate1^2 + sum_intermediate2^2)
SSWS <- sum(df2$sum_2)
MSWS <- SSWS/(n*(k-1))


# 4) Calculate versions of ICC (use formulas from Fig. 1)----
# ICC(1) = ICC_consistency = 
(MSBS - MSWS)/(MSBS + (k-1)*MSWS) # 0.7641323 # same result!



# in progress # 
# 5) psych::ICC----
df_ICC <- df %>% dplyr::select(-patcode)
wide_data <- df_ICC %>%
  pivot_wider(names_from = Index1, values_from = c(trans1, trans2))

psych::ICC(wide_data %>% dplyr::select(trans1_1, trans1_2))
# Reason for error???
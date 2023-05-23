# amost all angles in high dimension are 90degrees.

# test 
#x <- c(1,1)
#y <- c(1,-1)

# [base code was extended using GPT-4]

sim_number <- 500
n <- 10 # dim, try more...

angle <- rep(NA, sim_number)
for(i in 1:sim_number){
  x <- runif(n, -1,1)
  y <- runif(n, -1,1)
  angle[i] <- acos((x %*% y)/(norm(x, type="2")*norm(y, type="2") ))*180/pi
}
hist(angle)
quantile(angle, probs = c(0.005,0.995))



# automate:
library(ggplot2)
library(tidyr)
library(reshape2)

sim_number <- 500

n_values <- c(10, 100, 1000, 10000) # dim
angle_list <- list()

for (n in n_values) {
  angle <- rep(NA, sim_number)
  
  for(i in 1:sim_number){
    x <- runif(n, -1,1)
    y <- runif(n, -1,1)
    angle[i] <- acos((x %*% y)/(norm(x, type="2")*norm(y, type="2") ))*180/pi
  }
  
  angle_list[[paste0('n=',n)]] <- angle
}

angle_df <- do.call(cbind, angle_list)
angle_df <- as.data.frame(angle_df)
names(angle_df) <- n_values

angle_melted <- melt(angle_df, variable.name = "n", value.name = "angle")

ggplot(angle_melted, aes(x=angle)) +
  geom_histogram(bins=30, fill='blue', color='black', alpha=0.7) +
  facet_wrap(~n, scales="fixed") + # Change here
  xlab("Angle in degrees") +
  ylab("Frequency") +
  theme_minimal()

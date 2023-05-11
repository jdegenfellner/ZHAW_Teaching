# amost all angles in high dimension are 90degrees.

# test 
#x <- c(1,1)
#y <- c(1,-1)

# [base code was extended using GPT-4]

sim_number <- 500
n <- 10 # dim

angle <- rep(NA, sim_number)
for(i in 1:sim_number){
  x <- runif(n, -1,1)
  y <- runif(n, -1,1)
  angle[i] <- acos((x %*% y)/(norm(x, type="2")*norm(y, type="2") ))*180/pi
}
hist(angle)
quantile(angle, probs = c(0.005,0.995))


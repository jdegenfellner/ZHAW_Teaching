# Polychoric correlations

# Polychoric correlation assumes that each ordinal 
# data point represents a binned continuous value 
# from a normal distribution, and then tries to 
# estimate the correlation coefficient on that 
# assumption.

# in progress

# https://www.r-bloggers.com/2021/11/calculate-polychoric-correlation-in-r/
# better: https://www.r-bloggers.com/2021/02/how-does-polychoric-correlation-work-aka-ordinal-to-ordinal-correlation/


library(polycor)
company1 <- c(2, 2, 3, 1, 3, 2, 3, 3, 2, 1, 3, 1, 1, 3, 3, 1, 3, 1, 3, 2)
company2 <- c(1, 1, 2, 2, 2, 3, 2, 2, 2, 2, 3, 1, 2, 2,32, 1, 3, 1, 2, 3)

polychor(company1, company2)

library(DescTools)
CorPolychor(company1, company2)


# Calculate by hand for dichotomous variables
# https://www.statisticshowto.com/polychoric-correlation/

company1_d <- company1>=2
company2_d <- company2>=2

tab <- table(company1_d, company2_d)
A <- tab[1,1]
B <- tab[1,2]
C <- tab[2,1]
D <- tab[2,2]

cos(180/(sqrt(A*D/(B*C))+1))
CorPolychor(company1_d, company2_d) # not working yet


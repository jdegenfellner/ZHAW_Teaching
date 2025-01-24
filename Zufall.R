##Simulieren einer Beobachtung, die verzerrt und zufallsbehafter ist.
## X = T + b + E
##Annahme: Wahre Grösse T, Bias b und (Zufalls)-messfehler sigma. Das heisst, Zufallsfehler kommt aus einer Verteilung mit Standardabweichung=sigma.
Truth <- 40
b <- 4
sigma <- 2
## Beobachtung ist Summe aus T, b und E
X <- Truth + b + rnorm(n = 1, mean = 0, sd = sigma)  ##Eine Beobachtung
X
##################################################
X <- Truth + b + rnorm(n = 10000, mean = 0, sd = sigma)  ##zehntausend Beobachtungen
summary(X) ##Der Durchschnitt ist bei T+b, die Streuung bei sigma
mean(X)
sd(X)
par(mfrow = c(2,1)) ##zwei Plots in einer Graphik
hist(X, main = "um T+b verteilte Fehler, Histogramm")
abline(v = Ttruth + b, col = "blue", lwd = 3)
boxplot(X, horizontal = TRUE, main = "um T+b verteilte Fehler, Boxplot")
abline(v = Truth + b, col = "blue", lwd = 3)
abline(v = Truth, col = "red", lwd = 3)

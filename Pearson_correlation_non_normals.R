# Pearson correlation for non-normal distributions

library(copula)
rho <- 0.5

normalCop <- normalCopula(param = rho, dim = 2)

cor_vec <- rep(NA, 1000)

for(i in 1:1000){
  # Generieren von Daten aus der Kopula
  #set.seed(123) # Für Reproduzierbarkeit
  sampleSize <- 1000
  copulaSample <- rCopula(sampleSize, normalCop)
  
  # Umwandeln der Kopula-Daten in nicht-normalverteilte Daten
  # Hier verwenden wir die Beta-Verteilung als Beispiel
  xData <- qbeta(copulaSample[,1], shape1 = 2, shape2 = 5)
  yData <- qbeta(copulaSample[,2], shape1 = 2, shape2 = 5)
  
  # Berechnen des Pearson-Korrelationskoeffizienten
  cor_vec[i] <- cor(xData, yData, method = "pearson")
  
}

hist(cor_vec)
mean(cor_vec) # not too bad
quantile(cor_vec, probs = c(0.025,0.975))


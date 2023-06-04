# Intro Bayes with Coin flip:

# R-Skript zur Berechnung der frequentistischen und Bayes'schen Schätzungen 
# der Wahrscheinlichkeit von "Kopf" (1 Kopf unter n Würfen)

# Setzen Sie die Parameter der Beta-Prior-Verteilung
alpha_prior = 2
beta_prior = 2

# Setzen Sie die Anzahl der Würfe
n_values = c(3, 5, 10, 25, 100, 1000)

# Erstellen Sie einen Dataframe, um die Ergebnisse zu speichern
results = data.frame(n = integer(), frequentist = numeric(), bayesian = numeric())

# Führen Sie die Berechnungen für jede Anzahl von Würfen durch
for (n in n_values) {
  # Berechnen Sie die frequentistische Schätzung (Anteil der "Kopf"-Ergebnisse)
  frequentist_estimate = 1 / n
  
  # Aktualisieren Sie die Beta-Verteilungsparameter für die Bayes'sche Schätzung
  alpha_posterior = alpha_prior + 1
  beta_posterior = beta_prior + n - 1
  
  # Berechnen Sie die Bayes'sche Schätzung (Erwartungswert der Beta-Verteilung)
  bayesian_estimate = alpha_posterior / (alpha_posterior + beta_posterior)
  
  # Fügen Sie die Ergebnisse zum Dataframe hinzu
  results = rbind(results, data.frame(n = n, frequentist = frequentist_estimate, bayesian = bayesian_estimate))
}

# Ausgabe der Ergebnisse
print(results)

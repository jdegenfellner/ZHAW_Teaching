# GPT4:

# Schreibe mir ein R skript, das den zentralen Grenzverteilungssatz 
# illustriert. Man startet mit irgendeiner Verteilung (z.B. Gamma und 
# wählbaren Parametern) und einer sample size und einer Anzahl an Wiederholungen 
# und dann soll am Ende die verteilung von \frac{\bar{X}-\mu}{\sigma/\sqrt{n}} 
# geplottet werden (mit ggplot), sodass man sieht, dass es sich gegen eine 
# Standardnormalverteilung N(0,1) annähert.

# Set working directory to source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(pacman)
p_load(tidyverse)

# Einstellbare Parameter
sample_size <- 70   # Größe einer Stichprobe
repetitions <- 5000 # Anzahl der Wiederholungen

# Parameter der Gamma-Verteilung
shape <- 2 # Formparameter
rate <- 1  # Ratenparameter

# Erwartungswert (Mittelwert) und Standardabweichung der Gamma-Verteilung
mu <- shape / rate
sigma <- sqrt(shape / (rate ^ 2))

# Original-Gamma-Verteilung plotten
gamma_data <- rgamma(10000, shape = shape, rate = rate)
df_gamma <- data.frame(gamma_data)

ggplot(df_gamma, aes(x = gamma_data)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "blue", alpha = 0.7) +
  ggtitle(paste("Gamma-Verteilung mit Shape", shape, "und Rate", rate)) +
  xlab("Wert") +
  ylab("Dichte") +
  theme_minimal()


# Leerer Vektor für die Speicherung der Z-Werte
z_values <- c()

# Simulierung
for (i in 1:repetitions) {
  sample_data <- rgamma(sample_size, shape = shape, rate = rate)  # Ziehen einer Stichprobe
  sample_mean <- mean(sample_data)                                # Mittelwert der Stichprobe
  
  # Z-Wert berechnen
  z_value <- (sample_mean - mu) / (sigma / sqrt(sample_size))
  
  # Z-Wert in den Vektor hinzufügen
  z_values <- c(z_values, z_value)
}

# Daten für ggplot vorbereiten
df <- data.frame(z_values)

# Plot erstellen
ggplot(df, aes(x = z_values)) +
  geom_histogram(aes(y = after_stat(density)), bins = 50, fill = "blue", alpha = 0.7) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red", size = 1) +
  ggtitle(paste0("Zentrale Grenzverteilung: Illustration mit Gamma-Verteilung, n="),repetitions) +
  xlab(expression((bar(x) - mu) / (sigma / sqrt(n)))) +
  ylab("Dichte") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "white")) + 
  theme(plot.title = element_text(hjust = 0.5, size = 9))


ggsave(paste("./images/Z_Distribution_SampleSize_", sample_size, "_Repetitions_", repetitions, "_Shape_", shape, "_Rate_", rate,"n_",repetitions, ".png"))


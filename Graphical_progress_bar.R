library(pacman)
p_load(tidyverse, progress)

# Funktion zur Anzeige der Fortschrittsanzeige mit echter Berechnung
show_progress <- function(iterations, calc_function) {
  pb <- progress_bar$new(
    format = "  :current/:total [:bar] :percent in :elapsed",
    total = iterations,
    clear = FALSE
  )
  
  results <- numeric(iterations)  # Array zur Speicherung der Ergebnisse
  
  for (i in 1:iterations) {
    # Update der Fortschrittsanzeige
    pb$tick()
    
    # Durchführung der Berechnung
    results[i] <- calc_function(i)
    
    # Erzeugen und Anzeigen eines Balkendiagramms, um den Fortschritt zu visualisieren
    progress_df <- data.frame(
      status = factor("Completed", levels = c("Completed")),
      count = i,
      percent = i / iterations * 100
    )
    
    p <- ggplot(progress_df, aes(x = status, y = count, fill = status)) +
      geom_bar(stat = "identity", fill = "red") + # Färbung des Balkens auf rot setzen
      geom_text(aes(label = sprintf("%.1f%%", percent)), 
                position = position_stack(vjust = 0.5), 
                hjust = -0.1) +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(limits = c(0, iterations)) + # Konstante Skalierung der y-Achse
      ggtitle("Progress Bar") +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none"  # Legende entfernen
      )
    
    # Aktualisieren des Plots im Plots-Fenster
    print(p)
    
    # Kleine Verzögerung, um den Fortschritt sichtbar zu machen
    Sys.sleep(0.1)
  }
  
  return(results)
}

# Bsp - Berechnungsfunktion für die Quadrate natürlicher Zahlen
calc_square <- function(i) {
  n <- i + 44  # Verschiebung um 44, damit i = 1 zu n = 45 führt
  return(n^2)
}

# Aufruf der Funktion zur Anzeige der Fortschrittsanzeige mit echter Berechnung
show_progress(length(45:99), calc_square)

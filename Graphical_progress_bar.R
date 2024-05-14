library(ggplot2)
library(progress)

# Funktion zur Anzeige der Fortschrittsanzeige
show_progress <- function(iterations) {
  pb <- progress_bar$new(
    format = "  :current/:total [:bar] :percent in :elapsed",
    total = iterations,
    clear = FALSE
  )
  
  for (i in 1:iterations) {
    # Update der Fortschrittsanzeige
    pb$tick()
    
    # Erzeugen und Anzeigen eines Balkendiagramms, um den Fortschritt zu visualisieren
    progress_df <- data.frame(
      status = factor(c("Completed", "Remaining"), levels = c("Completed", "Remaining")),
      count = c(i, iterations - i)
    )
    
    p <- ggplot(progress_df, aes(x = status, y = count, fill = status)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(limits = c(0, iterations)) + # Konstante Skalierung der y-Achse
      ggtitle("Progress Bar") +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none"
      )
    
    # Aktualisieren des Plots im Plots-Fenster
    print(p)
    
    # Kleine Verzögerung, um den Fortschritt sichtbar zu machen
    Sys.sleep(0.1)
  }
}

# Anzahl der Iterationen
iterations <- 100

# Aufruf der Funktion zur Anzeige der Fortschrittsanzeige
show_progress(iterations)

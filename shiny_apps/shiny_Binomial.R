library(pacman)
p_load(shiny, ShinyApps)

# Erstelle die Shiny App
shinyApp(
  # Definiere die Benutzeroberfläche
  ui = fluidPage(
    titlePanel("Visualisierung der Binomialverteilung"),
    
    sidebarLayout(
      sidebarPanel(
        sliderInput("size", "Anzahl der Versuche (n):", 
                    min = 1, max = 100, value = 15),
        
        sliderInput("prob", "Wahrscheinlichkeit (π):", 
                    min = 0, max = 1, step = 0.1, value = 0.5)
      ),
      
      mainPanel(
        plotOutput("binomPlot")
      )
    )
  ),
  
# Definiere den Server-Teil
  server = function(input, output) {
    output$binomPlot <- renderPlot({
      # Erstelle einen Vektor von x-Werten
      x <- 0:input$size
      # Berechne die Binomialverteilung
      y <- dbinom(x, size=input$size, prob=input$prob)
      
      # Erstelle den Plot
      barplot(y, names.arg=x, main="Binomialverteilung", 
              xlab="x", ylab="Wahrscheinlichkeit", border="blue", col="grey", las=1)
      abline(h = 0, lty = 2)
    })
  }
)

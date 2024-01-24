##linreg
library(shiny)
ui <- fluidPage(
  withMathJax(),
    titlePanel("The Fundamental Model in Science: Simple linear model"),
    titlePanel("$$ Y_i=\\beta_1+\\beta_2 x_i+ N(0,\\sigma^2), i=1,...,n $$"),
    sidebarLayout(          
        sidebarPanel(
          titlePanel("Simulation from known truth"),
          sliderInput(inputId="beta0",label = "True Intercept", min = -100, max = 100, value =0),
          sliderInput(inputId="beta1",label = "True Slope", min = -100,max = 100,value =50),
          sliderInput(inputId="sigma",label = "True sigma:",min = 0,max = 50,value = 10)
          ),
        mainPanel(
          titlePanel("Analysis"),
          sliderInput(inputId="n",label = "Sample Size:",min = 30,max = 1000,value = 60),
                  actionButton(inputId = "refresh",label = "Refresh", icon = icon("fa fa-refresh")),
        verticalLayout("Red: Estimated expected value vor given predictor value", 
                       "Red dashed: Confidence bound for expected value given predictor value", 
                       "Blue: Prediction bound for future observation given predictor value"),
        plotOutput(outputId = "LMplot"),
        verbatimTextOutput(outputId="stats"),
        verbatimTextOutput(outputId="comp")
        )))
server <- function(input, output) {
    xfix<-seq(0,1,by=0.01)
    x<-reactive({input$refresh;runif(input$n,0,1)})
    y<-reactive({input$beta0+input$beta1*x()+rnorm(input$n,0,input$sigma)})
    mod<-reactive({lm(y()~1+x())})
    mod2<-reactive({lm(y()~1)})
    output$LMplot <- renderPlot(
    {
      plot(x(),y(),xlab="predictor",ylab="value")
      results<-data.frame(y=y(),x=x())
      lmod<-lm(y~x,data=results)
      abline(lmod,col="red",lwd=2)
      conf_interval <- predict(lmod,newdata=data.frame(x=xfix), interval="confidence",level = 0.95)
      lines(xfix, conf_interval[,2], col="red", lty=2)
      lines(xfix, conf_interval[,3], col="red", lty=2)
      pred_interval <- predict(lmod,newdata=data.frame(x=xfix),interval="prediction",level = 0.95)
      lines(xfix, pred_interval[,2], col="blue", lty=2)
      lines(xfix, pred_interval[,3], col="blue", lty=2)
    })
    output$stats<-renderPrint({summary(mod())})
    output$comp<-renderPrint({anova(mod2(),mod())})
}    
shinyApp(ui = ui, server = server)

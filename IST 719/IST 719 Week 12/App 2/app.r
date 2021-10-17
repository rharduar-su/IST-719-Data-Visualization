#
#file name: app.R
#Author: Rayanna Harduarsingh
# App 2 Directory

#install.packages("shiny")
library(shiny)

server <- function(input, output) {
  output$studentPlot <- renderPlot({
    hist(rnorm(n = 100), col = "orange")
  })
}

ui <- fluidPage(
  mainPanel("App2: a plot",
            plotOutput("studentPlot"))
)

shinyApp(ui = ui, server = server)
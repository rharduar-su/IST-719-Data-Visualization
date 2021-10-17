#
#file name: app.R
#Author: Rayanna Harduarsingh
#

#install.packages("shiny")
library(shiny)

server <- function(input, output) {
  
}

ui <- fluidPage(
  mainPanel("App1: Hello World")
)

shinyApp(ui = ui, server = server)


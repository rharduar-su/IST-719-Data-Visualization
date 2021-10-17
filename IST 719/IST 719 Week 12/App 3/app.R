#
#file name: app.R
#Author: Rayanna Harduarsingh
# App 3 Directory

#install.packages("shiny")
library(shiny)
library(ggplot2)
library(dplyr)

file.choose()
server <- function(input, output) {

  sales <- read.csv(paste0("/Users/rayannaharduarsingh/Desktop/data/sales.csv"), header = TRUE
                    , stringsAsFactors = FALSE)
  
  output$yearlySales <- renderPlot({
    sales %>% group_by(rep.region, year) %>%
      summarise(recipt = sum(recipt)) %>%
      ggplot() +
      aes(x = year, y = recipt, color = rep.region) +
      geom_line(size = 2) + ylim(c(0, 100000)) +
      ggtitle("Sales by Region") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

ui <- fluidPage(
  titlePanel("ACME Wine Company Dashboard"),
  
  mainPanel(
    plotOutput("yearlySales")
  )
)

shinyApp(ui = ui, server = server)









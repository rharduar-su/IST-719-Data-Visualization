#
#file name: app.R
#Author: Rayanna Harduarsingh
# App 4 Directory

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
  
  output$wine.by.region <- renderPlot({
    print("Inside wine.by.region")
    sales.2 <- sales
    region.name <- "All"
    my.year <- "All"
    
    if (input$region != "All") {
      print(paste("Inside wine.by.region: ", input$region))
      sales.2 <- sales %>% filter(rep.region == input$region)
      region.name <- input$region
    }
    if (input$region != "All") {
      print(paste("Inside wine.by.year: ", input$year))
      sales.2 <- sales %>% filter(year == input$year)
      my.year <- input$year
    }
    
    sales.2 %>% group_by(wine, type) %>%
      summarise(recipt = sum(recipt)) %>%
      ggplot() +
      aes(x = wine, y = recipt, fill = type) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("#B62D23", "#FBD16D")) +
      theme_minimal() +
      ggtitle(paste("Wine sales for region", region.name, ", year", my.year))
  })
}

ui <- fluidPage(
  titlePanel("ACME Wine Company Dashboard"),
  
  sidebarLayout(
    sidebarPanel( tags$style(".well {background-color:#A0A0A0}"),
      plotOutput("yearlySales")
    ),

  mainPanel(
    selectInput("region", "Select Region",
        choices = c("All", "Central", "East", "North", "South", "West")),
    selectInput("year", "Select Year",
        choices = c("All", "2010", "2011", "2012", "2013", "2014")),
    plotOutput("wine.by.region")
    )
  )
)

shinyApp(ui = ui, server = server)


#
# Fire Wise Gardens LA
# Shiny App Code
# Authors: Sarayu Ramnath, Nico Gavigan, Kyle Alves
# ESM- 244
# Bren W25
#

#install libraries if needed
#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("sf")
#install.packages("leaflet")
#install.packages("shinydashboard")
#install.packages("leaflet.extras")
#install.packages("plotly")
#install.packages("shinymaterial")
#install.packages("shinyWidgets")

#load libraries
library(here)
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(leaflet)
library(shinydashboard)
library(leaflet.extras)
library(plotly)
library(shinymaterial)
library(shinyWidgets)
library(janitor)

#load in data
#plant_data_raw <- read.csv(here("data", "full_california_native_plants_aligned.csv"))

plant_data_clean <- full_california_native_plants_aligned |>
  clean_names()
  
plant_data_clean

# Define UI
ui <- fluidPage(

    # Application title
    titlePanel("Fire Wise Gardens LA"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          # Widget 1: Plant Height Category
          selectInput("h_w", "Select Plant Height Category:",
                      choices = c("Tree", "Shrub", "Bush")),
          
          # Widget 2: Pollinator Interaction
          checkboxGroupInput("wildlife", "Select Pollinator Interactions:",
                             choices = c("Birds", "Bees", "Butterflies")),
          
          # Widget 3: Fire Resilience
          selectInput("water", "Select Fire Resilience Level:",
                      choices = c("Low", "Medium", "High")),
          
          # Widget 4: Nursery Availability
          checkboxInput("nursery_availability", "Show only plants available in LA nurseries", FALSE)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("filtered_table")
        )
    )
)

# Define server
server <- function(input, output) {
  # Reactive expression to filter data based on user input
  filtered_data <- reactive({
    data <- plant_data_clean
    
    # Filter by height category
    if (input$h_w != "") {
      data <- subset(data, h_w == input$h_w)
    }
    
    # Filter by pollinator interactions
    if (length(input$pollinators) > 0) {
      data <- subset(data, Pollinators %in% input$wildlife)
    }
    
    # Filter by fire resilience
    if (input$fire_resilience != "") {
      data <- subset(data, FireResilience == input$water)
    }
    
    # Filter by nursery availability
    if (input$nursery_availability) {
      # Not sure how we may be able to map out nursery availability?
    }
    
    return(data)
    })

  #Render the filtered data table
  output$filtered_table <- renderTable({
    filtered_data()
  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

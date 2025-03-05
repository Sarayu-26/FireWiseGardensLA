#playing around with the map layer and figuring out ways to input it into the shiny app 

library(shiny)
library(leaflet)
library(dplyr)
library(here)

# Define UI
ui <- fluidPage(
  # Title
  titlePanel("Native Plant Nurseries in LA"),
  
  # Widget to select a nursery
  selectInput("nursery_select", 
              "Choose a Nursery:", 
              choices = NULL),  # Will be populated dynamically
  
  # Map widget
  leafletOutput("map")  # Output placeholder for the map
)

# Define server logic
server <- function(input, output, session) {
  
  # Read the nurseries data from the CSV file in the 'data' folder
  nurseries <- read.csv(here("data", "nurseries_geocoded.csv"))
  
  # Populate the nursery selection dropdown with the nursery names
  updateSelectInput(session, "nursery_select", 
                    choices = unique(nurseries$name), 
                    selected = unique(nurseries$name)[1])
  
  # Reactive expression to filter nurseries based on selected nursery
  filtered_nursery <- reactive({
    nurseries %>% 
      filter(name == input$nursery_select)
  })
  
  # Render the leaflet map
  output$map <- renderLeaflet({
    leaflet(nurseries) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(lng = ~long, lat = ~lat, popup = ~name)
  })
  
  # Observe changes to the selected nursery and update the map markers
  observe({
    selected_nursery <- filtered_nursery()
    
    leafletProxy("map") %>%
      clearMarkers() %>%  # Clear existing markers
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data = selected_nursery, 
                 lng = ~long, lat = ~lat, popup = ~name)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

#further steps 
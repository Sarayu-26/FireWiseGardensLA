#playing around with the map layer and figuring out ways to input it into the shiny app 
library(shiny)
library(leaflet)
library(dplyr)
library(here)

species_by_nursery <- read.csv(here("data", "species_by_nursery.csv"))

# Define UI
ui <- fluidPage(
  titlePanel("Find Native Plants in LA Nurseries"),
  
  tabsetPanel(
    # Tab 1: Filter by Nursery
    tabPanel("Select by Nursery",
             selectInput("nursery_select", "Choose a Nursery:", choices = NULL),
             leafletOutput("nursery_map")
    ),
    
    # Tab 2: Filter by Plant Species
    tabPanel("Select by Plant Species",
             selectInput("species_select", "Choose a Plant Species:", choices = NULL),
             leafletOutput("species_map")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Populate dropdown for nursery selection based on species_by_nursery data
  observe({
    updateSelectInput(session, "nursery_select", 
                      choices = c("All Nurseries", unique(species_by_nursery$name)), 
                      selected = "All Nurseries")
  })
  
  # Populate dropdown for species selection based on species_by_nursery data
  observe({
    updateSelectInput(session, "species_select", 
                      choices = unique(species_by_nursery$species_name), 
                      selected = unique(species_by_nursery$species_name)[1])
  })
  
  # Reactive: Filter nurseries by selected nursery (handles "All Nurseries")
  filtered_nursery <- reactive({
    if (input$nursery_select == "All Nurseries") {
      # Return all nurseries if "All Nurseries" is selected
      return(species_by_nursery)
    } else {
      # Filter by the selected nursery
      return(species_by_nursery %>% filter(name == input$nursery_select))
    }
  })
  
  # Reactive: Filter species by selected plant species
  filtered_species <- reactive({
    species_by_nursery %>% filter(species_name == input$species_select)
  })
  
  # Render map for selected nursery
  output$nursery_map <- renderLeaflet({
    leaflet(species_by_nursery) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -118.2437, lat = 34.0522, zoom = 10)  # Center on LA
  })
  
  # Update markers for selected nursery
  observe({
    selected_nursery <- filtered_nursery()
    
    leafletProxy("nursery_map") %>%
      clearMarkers() %>%
      addMarkers(data = selected_nursery, 
                 lng = ~long, lat = ~lat, popup = ~paste(name, "<br>Plant:", species_name))
  })
  
  # Render map for selected plant species
  output$species_map <- renderLeaflet({
    leaflet(species_by_nursery) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -118.2437, lat = 34.0522, zoom = 10)  # Center on LA
  })
  
  # Update markers for selected plant species
  observe({
    selected_species <- filtered_species()
    
    # Get the corresponding nurseries for the selected plant species
    selected_nurseries <- species_by_nursery %>% 
      filter(species_name == input$species_select)
    
    leafletProxy("species_map") %>%
      clearMarkers() %>%
      addMarkers(data = selected_nurseries, 
                 lng = ~long, lat = ~lat, popup = ~paste(name, "<br>Plant:", species_name))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
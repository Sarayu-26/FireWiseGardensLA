#playing around with the map layer and figuring out ways to input it into the shiny app 
library(shiny)
library(leaflet)
library(dplyr)
library(here)
library(stringr)

# Load species_by_nursery dataset
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
    
    # Tab 2: Filter by Plant Species (Using common_name)
    tabPanel("Select by Plant Species",
             selectInput("species_select", "Choose a Plant Species:", choices = NULL),
             leafletOutput("species_map")
    )
  )
)

# Define Server Logic
server <- function(input, output, session) {
  
  # Populate dropdown for nursery selection
  observe({
    updateSelectInput(session, "nursery_select", 
                      choices = c("All Nurseries", unique(species_by_nursery$name)), 
                      selected = "All Nurseries")
  })
  
  # Populate dropdown for species selection (Now using `common_name`)
  observe({
    updateSelectInput(session, "species_select", 
                      choices = unique(species_by_nursery$common_name), 
                      selected = unique(species_by_nursery$common_name)[1])
  })
  
  # Reactive: Filter nurseries by selected nursery
  filtered_nursery <- reactive({
    if (input$nursery_select == "All Nurseries") {
      return(species_by_nursery %>%
               group_by(name) %>%
               summarize(lat = mean(lat), long = mean(long), .groups = "drop"))  # Ensures unique nurseries
    } else {
      return(species_by_nursery %>%
               filter(name == input$nursery_select) %>%
               group_by(name) %>%
               summarize(lat = mean(lat), long = mean(long), .groups = "drop"))  # Unique nursery
    }
  })
  
  # Reactive: Filter species by selected plant species (Using `common_name`)
  filtered_species <- reactive({
    species_by_nursery %>% filter(common_name == input$species_select)
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
      clearShapes() %>%
      addMarkers(data = selected_nursery, 
                 lng = ~long, lat = ~lat, popup = ~name)
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
    
    if (nrow(selected_species) > 0) {
      leafletProxy("species_map") %>%
        clearMarkers() %>%
        addMarkers(data = selected_species, 
                   lng = ~long, lat = ~lat, popup = ~paste(name, "<br>Plant:", common_name))
    } else {
      leafletProxy("species_map") %>%
        clearMarkers()
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
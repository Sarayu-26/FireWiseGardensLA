#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ggplot2)
library(caret)
library(DT)
library(ggmap)
library(sf)
library(tigris)
library(dplyr)
library(tidygeocoder)
library(leaflet)
library(here)
library(xtable)
library(stringr)  # For string manipulation
library(shiny)
library(leaflet)
library(dplyr)
library(here)
library(stringr)
library(htmltools)


# Load species_by_nursery dataset
species_by_nursery <- read.csv(here("data", "species_by_nursery.csv"))

# Define nursery websites
nursery_websites <- c(
  "Theodore Payne Nursery, Los Angeles" = "https://theodorepayne.org/plants-and-seeds/nursery/",
  "Artemisia Nursery, Los Angeles" = "https://artemisianursery.com/",
  "Matilija Nursery, Moorpark" = "https://www.matilijanursery.com/",
  "El Nativo Growers, Azusa" = "https://elnativogrowers.com/",
  "Plant Material, Los Angeles" = "https://plant-material.com/",
  "Lincoln Avenue Nursery, Pasadena" = "https://lincolnavenuenursery.com/"
)

# Clean the website column
species_by_nursery$website <- as.character(species_by_nursery$website)
species_by_nursery$website <- stringr::str_trim(species_by_nursery$website)
species_by_nursery$website <- iconv(species_by_nursery$website, to = "ASCII", sub = "")

# Load plant data
plant_data_full <- read.csv(here("data", "ca_plants_clean_chrctr.csv"), stringsAsFactors = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Fire Wise Gardens & Resilience Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Plant Selection' || input.tabs == 'Select by Nursery' || input.tabs == 'Select by Plant Species'",
        selectInput("fire_resistance", "Fire Resistance:", choices = c("Not Selected", "Yes", "No")),
        selectInput("growth_rate", "Growth Rate:", choices = c("Not Selected", "Slow", "Moderate", "Rapid")),
        sliderInput("height", "Height (cm):", 
                    min = ifelse(all(is.na(plant_data_full$height)), 0, min(plant_data_full$height, na.rm = TRUE)), 
                    max = ifelse(all(is.na(plant_data_full$height)), 229, max(plant_data_full$height, na.rm = TRUE)), 
                    value = c(min(plant_data_full$height, na.rm = TRUE), max(plant_data_full$height, na.rm = TRUE))),
        selectInput("drought_tolerance", "Drought Tolerance:", choices = c("Not Selected", unique(plant_data_full$drought_tolerance))),
        selectInput("fire_tolerance", "Fire Tolerance:", choices = c("Not Selected", unique(plant_data_full$fire_tolerance))),
        selectInput("moisture_use", "Moisture Use:", choices = c("Not Selected", unique(plant_data_full$moisture_use))),
        sliderInput("planting_density", "Planting Density:", 
                    min = ifelse(all(is.na(plant_data_full$planting_density)), 70, min(plant_data_full$planting_density, na.rm = TRUE)), 
                    max = ifelse(all(is.na(plant_data_full$planting_density)), 43560, max(plant_data_full$planting_density, na.rm = TRUE)), 
                    value = c(min(plant_data_full$planting_density, na.rm = TRUE), max(plant_data_full$planting_density, na.rm = TRUE))),
        sliderInput("root_depth", "Root Depth (cm):", 
                    min = ifelse(all(is.na(plant_data_full$root_depth)), 0, min(plant_data_full$root_depth, na.rm = TRUE)), 
                    max = ifelse(all(is.na(plant_data_full$root_depth)), 60, max(plant_data_full$root_depth, na.rm = TRUE)), 
                    value = c(min(plant_data_full$root_depth, na.rm = TRUE), max(plant_data_full$root_depth, na.rm = TRUE))),
        selectInput("bloom_period", "Bloom Period:", choices = c("Not Selected", sort(unique(plant_data_full$bloom_period)))),
        selectInput("growth_habit", "Growth Habit:", choices = c("Not Selected", sort(unique(plant_data_full$growth_habit)))),
        selectInput("growth_period", "Growth Period:", choices = c("Not Selected", sort(unique(plant_data_full$growth_period)))),
        checkboxInput("nursery_availability", "Show only plants available in LA nurseries", FALSE)
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Exploratory Data Analysis' || input.tabs == 'Model Training & Comparison'",
        selectInput("predictors", "Select Predictors:", 
                    choices = names(plant_data_full)[1:5], multiple = TRUE, selected = names(plant_data_full)[1:3]),
        actionButton("run_model", "Run Model", class = "btn-primary")
      )
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        
        tabPanel("Introduction",
                 h2("Welcome to Fire Wise Gardens!"),
                 p("This app helps you select fire-resilient plants for your garden in Los Angeles. Using data from the USDA Natural Resources Conservation Service Plant Database, we provide insights into plant characteristics and their fire resilience."),
                 br(),
                 h3("About the Data"),
                 p("The data used in this app is sourced from the USDA Natural Resources Conservation Service Plant Database. We filtered the data to include only plants native to or suitable for Los Angeles. The dataset includes the following variables:"),
                 tags$ul(
                   tags$li(strong("Fire Resistance:"), "Whether the plant is fire-resistant (Yes/No)."),
                   tags$li(strong("Growth Rate:"), "The growth rate of the plant (Slow, Moderate, Rapid)."),
                   tags$li(strong("Height:"), "The height of the plant in centimeters (ranging from 0 to 229 cm)."),
                   tags$li(strong("Drought Tolerance:"), "The plant's tolerance to drought (Low, Medium, High)."),
                   tags$li(strong("Fire Tolerance:"), "The plant's tolerance to fire (Low, Medium, High)."),
                   tags$li(strong("Moisture Use:"), "The plant's moisture requirements (Low, Medium, High)."),
                   tags$li(strong("Planting Density:"), "The recommended planting density (ranging from 70 to 43,560 plants per unit area)."),
                   tags$li(strong("Root Depth:"), "The depth of the plant's roots in centimeters (ranging from 0 to 60 cm)."),
                   tags$li(strong("Bloom Period:"), "The period during which the plant blooms (e.g., Spring, Summer)."),
                   tags$li(strong("Growth Habit:"), "The growth habit of the plant (e.g., Tree, Shrub, Forb/herb)."),
                   tags$li(strong("Growth Period:"), "The period during which the plant grows (e.g., Spring, Summer).")
                 ),
                 br(),
                 h3("Binary Logistic Regression"),
                 p("To determine whether a plant is fire-resilient, we use binary logistic regression, a supervised learning algorithm. This model predicts the probability of a binary outcome (e.g., fire-resilient or not) based on input features such as growth rate, height, and drought tolerance."),
                 p("The logistic regression model works by:"),
                 tags$ul(
                   tags$li("Fitting a logistic function to the data."),
                   tags$li("Estimating the probability of a plant being fire-resilient based on its characteristics."),
                   tags$li("Classifying the plant as fire-resilient if the probability exceeds a threshold (e.g., 0.5).")
                 ),
                 br(),
                 p("Explore the app to find the best plants for your garden and learn more about their fire resilience!")
        ),
        
        tabPanel("Plant Selection",
                 tableOutput("filtered_table")),
        
        tabPanel("Exploratory Data Analysis",
                 plotOutput("histogram"),
                 plotOutput("boxplot")),
        
        tabPanel("Model Training & Comparison",
                 verbatimTextOutput("model_summary"),
                 DTOutput("model_table"),
                 verbatimTextOutput("best_model")),
        
        tabPanel("Map Tabs", 
                 tabsetPanel(
                   tabPanel("Select by Nursery",
                            selectInput("nursery_select", "Choose a Nursery:", choices = NULL),
                            leafletOutput("nursery_map")),
                   
                   tabPanel("Select by Plant Species",
                            selectInput("species_select", "Choose a Plant Species:", choices = NULL),
                            leafletOutput("species_map"))
                 )
        )
      )
    )
  )
)

# Define Server
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
               summarize(lat = mean(lat), long = mean(long), .groups = "drop"))
    } else {
      return(species_by_nursery %>%
               filter(name == input$nursery_select) %>%
               group_by(name) %>%
               summarize(lat = mean(lat), long = mean(long), .groups = "drop"))
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
      setView(lng = -118.2437, lat = 34.0522, zoom = 10)
  })
  
  # Update markers for selected nursery
  observe({
    selected_nursery <- filtered_nursery()
    print(selected_nursery) 
    
    selected_nursery$website <- nursery_websites[selected_nursery$name]
    print(selected_nursery$website)
    
    leafletProxy("nursery_map") %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(data = selected_nursery, 
                 lng = ~long, lat = ~lat, 
                 popup = ~paste(name, "<br><a href='", website, "' target='_blank'>Visit Website</a>"))
  })
  
  # Render map for selected plant species
  output$species_map <- renderLeaflet({
    leaflet(species_by_nursery) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -118.2437, lat = 34.0522, zoom = 10)
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
  
  # Reactive expression for filtering plant data
  filtered_data <- reactive({
    data <- plant_data_full
    
    if (input$fire_resistance != "Not Selected") {
      data <- subset(data, fire_resistance == input$fire_resistance)
    }
    
    if (input$growth_rate != "Not Selected") {
      data <- subset(data, growth_rate == input$growth_rate)
    }
    
    if (input$height[1] != 0 || input$height[2] != 229) {
      data <- subset(data, height >= input$height[1] & height <= input$height[2])
    }
    
    if (input$drought_tolerance != "Not Selected") {
      data <- subset(data, drought_tolerance == input$drought_tolerance)
    }
    
    if (input$fire_tolerance != "Not Selected") {
      data <- subset(data, fire_tolerance == input$fire_tolerance)
    }
    
    if (input$moisture_use != "Not Selected") {
      data <- subset(data, moisture_use == input$moisture_use)
    }
    
    if (input$planting_density[1] != 70 || input$planting_density[2] != 43560) {
      data <- subset(data, planting_density >= input$planting_density[1] & planting_density <= input$planting_density[2])
    }
    
    if (input$root_depth[1] != 0 || input$root_depth[2] != 60) {
      data <- subset(data, root_depth >= input$root_depth[1] & root_depth <= input$root_depth[2])
    }
    
    if (input$bloom_period != "Not Selected") {
      data <- subset(data, bloom_period == input$bloom_period)
    }
    
    if (input$growth_habit != "Not Selected") {
      data <- subset(data, growth_habit == input$growth_habit)
    }
    
    if (input$growth_period != "Not Selected") {
      data <- subset(data, growth_period == input$growth_period)
    }
    
    if (input$nursery_availability) {
      data <- subset(data, nursery_available == TRUE)
    }
    
    return(data)
  })
  
  # Render the filtered data table with formatted column names
  output$filtered_table <- renderTable({
    filtered_data() %>%
      rename_with(~ str_replace_all(., "_", " ") %>% str_to_title())
  })
}
# Run the application 
shinyApp(ui, server)
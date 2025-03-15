library(shiny)
library(shinythemes)
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
library(stringr)
library(htmltools)
library(tidymodels)
library(readr)
library(scales)

# ===============================
# Data Loading & Preprocessing
# ===============================

# Load nursery data & plant data for mapping & filtering
species_by_nursery <- read.csv(here("data", "species_by_nursery.csv"))
species_by_nursery$website <- as.character(species_by_nursery$website)
species_by_nursery$website <- stringr::str_trim(species_by_nursery$website)
species_by_nursery$website <- iconv(species_by_nursery$website, to = "ASCII", sub = "")

plant_data_full <- read.csv(here("data", "ca_plants_clean_chrctr.csv"), stringsAsFactors = FALSE)

# Define nursery websites lookup
nursery_websites <- c(
  "Theodore Payne Nursery, Los Angeles" = "https://theodorepayne.org/plants-and-seeds/nursery/",
  "Artemisia Nursery, Los Angeles" = "https://artemisianursery.com/",
  "Matilija Nursery, Moorpark" = "https://www.matilijanursery.com/",
  "El Nativo Growers, Azusa" = "https://elnativogrowers.com/",
  "Plant Material, Los Angeles" = "https://plant-material.com/",
  "Lincoln Avenue Nursery, Pasadena" = "https://lincolnavenuenursery.com/"
)

# Convert plant height values from cm to inches for UI display (Plant Selection tab)
min_height_cm <- ifelse(all(is.na(plant_data_full$height)), 0, min(plant_data_full$height, na.rm = TRUE))
max_height_cm <- ifelse(all(is.na(plant_data_full$height)), 229, max(plant_data_full$height, na.rm = TRUE))
min_height_in <- round(min_height_cm / 2.54, 1)
max_height_in <- round(max_height_cm / 2.54, 1)

# -------------------------------
# Load data for Logistic Regression
# -------------------------------
all_plants <- read_csv(here::here("data", "all_plants.csv"))
plant_df <- all_plants %>%
  mutate(
    fire_resistance = factor(fire_resistance),
    growth_rate = factor(growth_rate),
    drought_tolerance = factor(drought_tolerance),
    fire_tolerance = factor(fire_tolerance),
    moisture_use = factor(moisture_use),
    bloom_period = factor(bloom_period),
    growth_habit = factor(growth_habit),
    growth_period = factor(growth_period)
  )

# For the Fire Resistance Calculator, convert height values to inches for UI display
min_height_cm_model <- min(plant_df$height, na.rm = TRUE)
max_height_cm_model <- max(plant_df$height, na.rm = TRUE)
mean_height_cm_model <- mean(plant_df$height, na.rm = TRUE)
min_height_in_model <- round(min_height_cm_model / 2.54, 1)
max_height_in_model <- round(max_height_cm_model / 2.54, 1)
mean_height_in_model <- round(mean_height_cm_model / 2.54, 1)

# Define planting density statistics for the model
min_pd_model <- min(plant_df$planting_density, na.rm = TRUE)
max_pd_model <- max(plant_df$planting_density, na.rm = TRUE)
mean_pd_model <- round(mean(plant_df$planting_density, na.rm = TRUE), 0)

# Split data for model training
set.seed(123)
plant_split <- initial_split(plant_df, prop = 0.8, strata = fire_resistance)
plant_train_df <- training(plant_split)
plant_test_df <- testing(plant_split)

# -------------------------------
# New Recipe for Logistic Regression
# -------------------------------
rec2 <- recipe(fire_resistance ~ moisture_use + growth_period + height + planting_density + root_depth, data = plant_train_df) |>
  step_normalize(height, planting_density, root_depth)

# Define logistic regression model specification
log_md <- logistic_reg() %>%
  set_engine("glm")

# Create and fit the workflow
log_fit2 <- workflow() %>%
  add_recipe(rec2) %>%
  add_model(log_md) %>%
  fit(plant_train_df)

# ====================================
# Define the Combined Shiny UI
# ====================================
ui <- fluidPage(
  theme = shinytheme("cerulean"),
  titlePanel("Fire Wise Gardens & Resilience Prediction"),
  sidebarLayout(
    sidebarPanel(
      # Controls for Plant Selection and Mapping tabs
      conditionalPanel(
        condition = "input.tabs == 'Plant Selection' || input.tabs == 'Select by Nursery' || input.tabs == 'Select by Plant Species'",
        selectInput("fire_resistance", "Fire Resistance:", choices = c("Not Selected", "Yes", "No")),
        selectInput("growth_rate", "Growth Rate:", choices = c("Not Selected", "Slow", "Moderate", "Rapid")),
        sliderInput("height", "Height (inches):", 
                    min = min_height_in,
                    max = max_height_in,
                    value = c(min_height_in, max_height_in)),
        selectInput("drought_tolerance", "Drought Tolerance:", choices = c("Not Selected", unique(plant_data_full$drought_tolerance))),
        selectInput("fire_tolerance", "Fire Tolerance:", choices = c("Not Selected", unique(plant_data_full$fire_tolerance))),
        selectInput("moisture_use_full", "Moisture Use:", choices = c("Not Selected", unique(plant_data_full$moisture_use))),
        sliderInput("planting_density", "Planting Density:", 
                    min = ifelse(all(is.na(plant_data_full$planting_density)), 70, min(plant_data_full$planting_density, na.rm = TRUE)), 
                    max = ifelse(all(is.na(plant_data_full$planting_density)), 43560, max(plant_data_full$planting_density, na.rm = TRUE)), 
                    value = c(min(plant_data_full$planting_density, na.rm = TRUE), max(plant_data_full$planting_density, na.rm = TRUE))),
        sliderInput("root_depth_full", "Root Depth (cm):", 
                    min = ifelse(all(is.na(plant_data_full$root_depth)), 0, min(plant_data_full$root_depth, na.rm = TRUE)), 
                    max = ifelse(all(is.na(plant_data_full$root_depth)), 60, max(plant_data_full$root_depth, na.rm = TRUE)), 
                    value = c(min(plant_data_full$root_depth, na.rm = TRUE), max(plant_data_full$root_depth, na.rm = TRUE))),
        selectInput("bloom_period", "Bloom Period:", choices = c("Not Selected", sort(unique(plant_data_full$bloom_period)))),
        selectInput("growth_habit", "Growth Habit:", choices = c("Not Selected", sort(unique(plant_data_full$growth_habit)))),
        selectInput("growth_period_full", "Growth Period:", choices = c("Not Selected", sort(unique(plant_data_full$growth_period)))),
        checkboxInput("nursery_availability", "Show only plants available in LA nurseries", FALSE)
      ),
      
      # Sidebar controls for the Fire Resistance Calculator tab
      conditionalPanel(
        condition = "input.tabs == 'Fire Resistance Calculator'",
        h3("Adjust Predictor Variables"),
        selectInput("moisture_use_calc", "Moisture Use:",
                    choices = levels(plant_df$moisture_use)),
        selectInput("growth_period_calc", "Growth Period:",
                    choices = levels(plant_df$growth_period)),
        numericInput("height_calc", "Plant Height (inches):", 
                     value = mean_height_in_model,
                     min = min_height_in_model, 
                     max = max_height_in_model, 
                     step = 1),
        numericInput("root_depth_calc", "Root Depth (inches):", 
                     value = round(mean(plant_df$root_depth, na.rm = TRUE), 1),
                     min = min(plant_df$root_depth, na.rm = TRUE), 
                     max = max(plant_df$root_depth, na.rm = TRUE), 
                     step = 0.5),
        numericInput("planting_density_calc", "Planting Density:", 
                     value = mean_pd_model,
                     min = min_pd_model, 
                     max = max_pd_model, 
                     step = 1),
        actionButton("predict_btn", "Predict Fire Resistance", class = "btn-primary")
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
                   tags$li(strong("Height:"), "The height of the plant (in inches)."),
                   tags$li(strong("Drought Tolerance:"), "The plant's tolerance to drought (Low, Medium, High)."),
                   tags$li(strong("Fire Tolerance:"), "The plant's tolerance to fire (Low, Medium, High)."),
                   tags$li(strong("Moisture Use:"), "The plant's moisture requirements (Low, Medium, High)."),
                   tags$li(strong("Planting Density:"), "The recommended planting density."),
                   tags$li(strong("Root Depth:"), "The depth of the plant's roots (in cm)."),
                   tags$li(strong("Bloom Period:"), "The period during which the plant blooms."),
                   tags$li(strong("Growth Habit:"), "The growth habit of the plant (e.g., Tree, Shrub, Forb/herb)."),
                   tags$li(strong("Growth Period:"), "The period during which the plant grows.")
                 ),
                 br(),
                 h3("Binary Logistic Regression"),
                 p("We use binary logistic regression to predict if a plant is fire resistant. The model uses predictors such as moisture use, growth period, plant height, planting density, and root depth to estimate the probability of fire resistance.")
        ),
        
        tabPanel("Plant Selection",
                 tableOutput("filtered_table")),
        
        tabPanel("Map Tabs", 
                 tabsetPanel(
                   tabPanel("Select by Nursery",
                            selectInput("nursery_select", "Choose a Nursery:", choices = NULL),
                            leafletOutput("nursery_map")),
                   
                   tabPanel("Select by Plant Species",
                            selectInput("species_select", "Choose a Plant Species:", choices = NULL),
                            leafletOutput("species_map"))
                 )
        ),
        
        tabPanel("Fire Resistance Calculator",
                 h3("Fire Resistance Prediction"),
                 p("Adjust the inputs below to predict the probability that a plant is fire resistant. The model calculates probabilities for both outcomes (Yes/No) and provides an overall classification based on a 0.5 threshold."),
                 br(),
                 tableOutput("prediction_table"))
      )
    )
  )
)

# ====================================
# Define the Combined Server Logic
# ====================================
server <- function(input, output, session) {
  
  # --------------
  # Original App Code: Mapping & Plant Selection
  # --------------
  
  # Populate dropdown for nursery selection
  observe({
    updateSelectInput(session, "nursery_select", 
                      choices = c("All Nurseries", unique(species_by_nursery$name)), 
                      selected = "All Nurseries")
  })
  
  # Populate dropdown for species selection (using common_name)
  observe({
    updateSelectInput(session, "species_select", 
                      choices = unique(species_by_nursery$common_name), 
                      selected = unique(species_by_nursery$common_name)[1])
  })
  
  # Reactive: Filter nurseries by selected nursery
  filtered_nursery <- reactive({
    if (input$nursery_select == "All Nurseries") {
      species_by_nursery %>%
        group_by(name) %>%
        summarize(lat = mean(lat), long = mean(long), .groups = "drop")
    } else {
      species_by_nursery %>%
        filter(name == input$nursery_select) %>%
        group_by(name) %>%
        summarize(lat = mean(lat), long = mean(long), .groups = "drop")
    }
  })
  
  # Reactive: Filter species by selected plant species
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
    selected_nursery$website <- nursery_websites[selected_nursery$name]
    
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
  
  # Reactive expression for filtering plant data (for Plant Selection tab)
  filtered_data <- reactive({
    data <- plant_data_full
    if (input$fire_resistance != "Not Selected") {
      data <- subset(data, fire_resistance == input$fire_resistance)
    }
    if (input$growth_rate != "Not Selected") {
      data <- subset(data, growth_rate == input$growth_rate)
    }
    # Convert the selected height in inches back to cm for filtering
    if (input$height[1] != min_height_in || input$height[2] != max_height_in) {
      lower_bound_cm <- input$height[1] * 2.54
      upper_bound_cm <- input$height[2] * 2.54
      data <- subset(data, height >= lower_bound_cm & height <= upper_bound_cm)
    }
    if (input$drought_tolerance != "Not Selected") {
      data <- subset(data, drought_tolerance == input$drought_tolerance)
    }
    if (input$fire_tolerance != "Not Selected") {
      data <- subset(data, fire_tolerance == input$fire_tolerance)
    }
    if (input$moisture_use_full != "Not Selected") {
      data <- subset(data, moisture_use == input$moisture_use_full)
    }
    if (input$planting_density[1] != min(plant_data_full$planting_density, na.rm = TRUE) || 
        input$planting_density[2] != max(plant_data_full$planting_density, na.rm = TRUE)) {
      data <- subset(data, planting_density >= input$planting_density[1] & planting_density <= input$planting_density[2])
    }
    if (input$root_depth_full[1] != min(plant_data_full$root_depth, na.rm = TRUE) || 
        input$root_depth_full[2] != max(plant_data_full$root_depth, na.rm = TRUE)) {
      data <- subset(data, root_depth >= input$root_depth_full[1] & root_depth <= input$root_depth_full[2])
    }
    if (input$bloom_period != "Not Selected") {
      data <- subset(data, bloom_period == input$bloom_period)
    }
    if (input$growth_habit != "Not Selected") {
      data <- subset(data, growth_habit == input$growth_habit)
    }
    if (input$growth_period_full != "Not Selected") {
      data <- subset(data, growth_period == input$growth_period_full)
    }
    if (input$nursery_availability) {
      data <- subset(data, nursery_available == TRUE)
    }
    data
  })
  
  output$filtered_table <- renderTable({
    filtered_data() %>%
      rename_with(~ str_replace_all(., "_", " ") %>% str_to_title())
  })
  
  # --------------
  # Fire Resistance Calculator (Binary Logistic Regression)
  # --------------
  
  pred_result <- eventReactive(input$predict_btn, {
    # Build a new data frame for prediction; convert height from inches back to cm for the model
    new_data <- tibble(
      moisture_use = factor(input$moisture_use_calc, levels = levels(plant_df$moisture_use)),
      growth_period = factor(input$growth_period_calc, levels = levels(plant_df$growth_period)),
      height = as.numeric(input$height_calc) * 2.54,
      root_depth = as.numeric(input$root_depth_calc),
      planting_density = as.numeric(input$planting_density_calc)
    )
    
    # Obtain predicted probabilities from the fitted model
    prob_preds <- predict(log_fit2, new_data = new_data, type = "prob")
    class_pred <- predict(log_fit2, new_data = new_data)
    
    # Extract probability for fire resistance = "Yes" and calculate probability for "No"
    prob_yes <- prob_preds$.pred_Yes
    prob_no <- prob_preds$.pred_No
    overall <- ifelse(prob_yes >= 0.5, "Yes", "No")
    
    # Create a formatted results table
    result <- tibble(
      `Fire Resistant?` = overall,
      `Probability (Yes)` = percent(prob_yes),
      `Probability (No)` = percent(prob_no)
    )
    result
  })
  
  output$prediction_table <- renderTable({
    pred_result()
  }, striped = TRUE, hover = TRUE)
}

# ====================================
# Run the Combined Application
# ====================================
shinyApp(ui, server)
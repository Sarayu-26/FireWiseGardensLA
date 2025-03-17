# Detach the infer package if it is loaded to avoid conflicts
if ("infer" %in% (.packages())) detach("package:infer", unload = TRUE)

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
library(tidymodels)
library(readr)
library(leaflet)
library(here)
library(xtable)
library(stringr)
library(htmltools)
library(readr)
library(scales)
library(forcats)


# ===============================
# Data Loading & Preprocessing
# ===============================

# Load nursery data & plant data for mapping & filtering
species_by_nursery <- read.csv(here("data", "species_by_nursery.csv"))
species_by_nursery$website <- as.character(species_by_nursery$website)
species_by_nursery$website <- stringr::str_trim(species_by_nursery$website)
species_by_nursery$website <- iconv(species_by_nursery$website, to = "ASCII", sub = "")

plant_data_full <- read.csv(here("data", "plant_data_full.csv"), stringsAsFactors = FALSE) #saved plant_data_full.csv from clean_chrctr and added new nursery column

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

# Helper function to clean up variable names
clean_labels <- function(variable) {
  recoded <- dplyr::recode(variable,
                           `fire_toleranceLow` = "Fire Tolerance (Low)",
                           `fire_toleranceMedium` = "Fire Tolerance (Medium)",
                           `fire_toleranceHigh` = "Fire Tolerance (High)",
                           `fire_toleranceNone` = "Fire Tolerance (None)",
                           `growth_rateSlow` = "Growth Rate (Slow)",
                           `growth_rateModerate` = "Growth Rate (Moderate)",
                           `growth_rateRapid` = "Growth Rate (Rapid)",
                           `drought_toleranceLow` = "Drought Tolerance (Low)",
                           `drought_toleranceMedium` = "Drought Tolerance (Medium)",
                           `drought_toleranceHigh` = "Drought Tolerance (High)",
                           `drought_toleranceNone` = "Drought Tolerance (None)",
                           `moisture_useLow` = "Moisture Use (Low)",
                           `moisture_useMedium` = "Moisture Use (Medium)",
                           `moisture_useHigh` = "Moisture Use (High)",
                           `bloom_periodEarlySpring` = "Bloom Period (Early Spring)",
                           `bloom_periodMidSpring` = "Bloom Period (Mid Spring)",
                           `bloom_periodLate Spring` = "Bloom Period (Late Spring)",
                           `bloom_periodSummer` = "Bloom Period (Summer)",
                           `bloom_periodWinter` = "Bloom Period (Winter)",
                           `bloom_periodSpring` = "Bloom Period (Spring)",
                           `bloom_periodMid Summer` = "Bloom Period (Mid Summer)",
                           `bloom_periodMid Spring` = "Bloom Period (Mid Spring)",
                           `bloom_periodLate Winter` = "Bloom Period (Late Winter)",
                           `bloom_periodLate Summer` = "Bloom Period (Late Summer)",
                           `bloom_periodIndeterminate` = "Bloom Period (Indeterminate)",
                           `bloom_periodFall` = "Bloom Period (Fall)",
                           `bloom_periodEarly Summer` = "Bloom Period (Early Summer)",
                           `growth_habitTree` = "Growth Habit (Tree)",
                           `growth_habitVine` = "Growth Habit (Vine)",
                           `growth_habitShrub` = "Growth Habit (Shrub)",
                           `growth_habitForbHerb` = "Growth Habit (Forb/Herb)",
                           `growth_habitGraminoid` = "Growth Habit (Graminoid)",
                           `growth_habitSubshrub` = "Growth Habit (Subshrub)",
                           `growth_periodSpring` = "Growth Period (Spring)",
                           `growth_periodSummer` = "Growth Period (Summer)",
                           `growth_periodFall` = "Growth Period (Fall)",
                           `growth_periodWinter` = "Growth Period (Winter)",
                           `growth_periodYear Round` = "Growth Period (Year Round)",
                           
  )
  
  return(recoded)
}

# -------------------------------
# Load data for Logistic Regression
# -------------------------------
all_plants <- read_csv(here::here("data", "all_plants.csv"))
plant_df <- all_plants %>%
  rename(species_id = `...1`)|>
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

# Split data for model training
set.seed(123)
plant_split <- initial_split(plant_df, prop = 0.8, strata = fire_resistance)
plant_train_df <- training(plant_split)
plant_test_df <- testing(plant_split)

# -------------------------------
# Updated Recipe for Logistic Regression using %>%
# -------------------------------
rec2 <- recipe(fire_resistance ~ moisture_use + growth_period + height + root_depth + growth_habit, 
               data = plant_train_df) %>%
  step_normalize(height, root_depth)

# Define logistic regression model specification
log_md <- logistic_reg() %>%
  set_engine("glm")

# Create and fit the workflow
log_fit2 <- workflow() %>%
  add_recipe(rec2) %>%
  add_model(log_md) %>%
  fit(plant_train_df)

# ====================================
# Define the Combined Shiny UI with Updated Aesthetics
# ====================================
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Montserrat"),
    tags$style(HTML("
      body {
        font-family: 'Montserrat', sans-serif;
        background-color: #f0f5f0;
      }
      .navbar, .well, .btn-primary {
        border-radius: 0;
      }
      .navbar {
        background-color: #4c6b64 !important;
        border-color: #3a574f !important;
      }
      .btn-primary {
        background-color: #4c6b64 !important;
        border-color: #3a574f !important;
      }
      h2, h3 {
        color: #3e5641;
      }
      .sidebar {
        background-color: #d9e4dd;
        padding: 15px;
        border-radius: 5px;
      }
      .tab-content {
        background-color: #ffffff;
        padding: 15px;
        border-radius: 5px;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
    "))
  ),
  
  titlePanel("Fire Wise Gardens & Resilience Prediction"),
  
  sidebarLayout(
    sidebarPanel(width = 2,
              
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
        selectInput("moisture_use_calc", "Moisture Use:", choices = levels(plant_df$moisture_use)),
        selectInput("growth_period_calc", "Growth Period:", choices = levels(plant_df$growth_period)),
        numericInput("height_calc", "Plant Height (inches):", 
                     value = mean_height_in_model,
                     min = min_height_in_model, 
                     max = max_height_in_model, 
                     step = 1),
        numericInput("root_depth_calc", "Root Depth (inches):", 
                     value = round(mean(plant_df$root_depth, na.rm = TRUE) / 2.54, 1),
                     min = round(min(plant_df$root_depth, na.rm = TRUE) / 2.54, 1), 
                     max = round(max(plant_df$root_depth, na.rm = TRUE) / 2.54, 1), 
                     step = 0.5),
        selectInput("growth_habit_calc", "Growth Habit:", choices = levels(plant_df$growth_habit)),
        actionButton("predict_btn", "Predict Fire Resistance", class = "btn-primary")
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  
                  tabPanel("Introduction",
                           fluidRow(
                             column(6, 
                                    div(
                                      style = "text-align: center;",
                                      tags$img(src = "ca_garden.jpg", width = "90%", style = "border-radius: 10px; box-shadow: 2px 2px 5px rgba(0,0,0,0.3);"),
                                      tags$figcaption("Photo: Summer Chaparral garden by Philip van Soelen", class = "image-credit")
                                    )
                             ),
                           column(10, 
                           h2("Welcome to Fire Wise Gardens!"),
                           p("This app helps you select fire-resilient plants for your garden in Los Angeles, CA. Using data from the USDA Natural Resources Conservation Service Plant Database, we provide insights into plant characteristics and their fire resilience."),
                           h3("About the Data"),
                           p("The data used in this app is sourced from the USDA Natural Resources Conservation Service Plant Database. We filtered the data to include only plants native to or suitable for Los Angeles. The dataset includes the following variables:"),
                           tags$ul(
                             tags$li(strong("Fire Resistance:"), "Yes/No."),
                             tags$li(strong("Growth Rate:"), "Slow, Moderate, Rapid."),
                             tags$li(strong("Height:"), "Plant height in inches."),
                             tags$li(strong("Drought Tolerance:"), "Low, Medium, High."),
                             tags$li(strong("Fire Tolerance:"), "Low, Medium, High."),
                             tags$li(strong("Moisture Use:"), "Low, Medium, High."),
                             tags$li(strong("Planting Density:"), "Recommended planting density."),
                             tags$li(strong("Root Depth:"), "Depth of roots in cm."),
                             tags$li(strong("Bloom Period:"), "When the plant blooms."),
                             tags$li(strong("Growth Habit:"), "Tree, Shrub, Forb/herb."),
                             tags$li(strong("Growth Period:"), "Active growth period.")
                           ),
                           br(),
                           h3("Data Cited and Acknowledgements"),
                           p("Thank you to Gerry at USDA for providing the data. PLANTS Database. United States Department of Agriculture. Accessed February 18 2025, from https://plants.usda.gov/csvdownload?plantLst=NRCSStateList&nrcsstate=California.")
                           )  
                           )
                  ),   
                  
                  tabPanel("Plant Selection", tableOutput("filtered_table")),
                  
                  tabPanel("Data Exploration",
                           sidebarLayout(
                             sidebarPanel(
                               selectInput("selected_variable", "Select Variable to Explore:",
                                           choices = c("growth_rate", "drought_tolerance", 
                                                       "fire_tolerance", "moisture_use", "bloom_period",
                                                       "growth_habit", "growth_period"),
                                           selected = "growth_rate")
                             ),
                             mainPanel(
                               plotOutput("bar_plot"),
                               tableOutput("summary_table"),
                               h4("Logistic Regression Results"),
                               verbatimTextOutput("logistic_summary"),
                               plotOutput("odds_ratio_plot")
                             )
                           )
                  ),
                  
                  tabPanel("Explore Local Nurseries",
                           fluidRow(
                             column(10,
                                    div(
                                      style = "text-align: center;",
                                      tags$img(src = "flower_mosaic.jpg", width = "85%", style = "border-radius: 10px; box-shadow: 2px 2px 5px rgba(0,0,0,0.3);"),
                                      tags$figcaption("Photo: CDFW Flower Mosaic Banner", class = "image-credit")
                                    )
                             ),
                             column(12, 
                           tabsetPanel(
                             tabPanel("Select by Nursery",
                                      selectInput("nursery_select", "Choose a Nursery:", choices = NULL),
                                      leafletOutput("nursery_map")
                             ),
                             tabPanel("Select by Plant Species",
                                      selectInput("species_select", "Choose a Plant Species:", choices = NULL),
                                      leafletOutput("species_map")
                             )
                           )
                           ) 
                           )
                  ),  
                  
                  tabPanel("Fire Resistance Calculator",
                           h3("Fire Resistance Prediction"),
                           tableOutput("prediction_table")
                  )
      ) 
    ) 
  ) 
)


# ====================================
# Define the Combined Server Logic
# ====================================
server <- function(input, output, session) {
  
  # --------------
  # Plant Selection Display 
  # --------------
  plant_df_display <- plant_df %>%
    rename_with(~ str_to_title(str_replace_all(., "_", " ")), everything())  # Capitalize column names
  
  # 4. Output the filtered table with Title Case columns
  output$filtered_table <- renderTable({
    plant_df_display  # Use the display version with Title Case column names
  }, striped = TRUE, hover = TRUE)
  
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
      data <- subset(data, nursery_availability == 1)
    }
    data
  })
  
  output$filtered_table <- renderTable({
    filtered_data() %>%
      rename_with(~ str_replace_all(., "_", " ") %>% str_to_title())
  })
  
  # --------------
  # Data Exploration: Logistic Regression
  # --------------
  
  # Convert selected variable to a factor and filter necessary columns
  selected_data <- reactive({
    plant_data_full %>%
      select(fire_resistance, all_of(input$selected_variable)) %>%
      mutate(across(everything(), as.factor))
  })
  
  # Bar plot for selected categorical variable ordered by frequency
  output$bar_plot <- renderPlot({
    # Reorder the selected variable by frequency
    selected_data_reordered <- selected_data() %>%
      mutate(ordered_variable = forcats::fct_infreq(.data[[input$selected_variable]]))
    
    ggplot(selected_data_reordered, aes(x = ordered_variable)) +
      geom_bar(fill = "steelblue", color = "black") +
      labs(title = paste("Distribution of", clean_labels(input$selected_variable)),
           x = clean_labels(input$selected_variable), y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Summary table for category counts with cleaned labels
  output$summary_table <- renderTable({
    selected_data() %>%
      mutate(across(all_of(input$selected_variable), clean_labels)) %>%
      group_by(.data[[input$selected_variable]]) %>%
      summarise(Count = n(), .groups = "drop")
  })
  
  # Logistic Regression Model
  logistic_model <- reactive({
    formula_str <- paste("fire_resistance ~", input$selected_variable)
    glm(as.formula(formula_str), data = selected_data(), family = binomial)
  })
  
  # Display Model Summary
  output$logistic_summary <- renderPrint({
    summary(logistic_model())
  })
  
  # Odds Ratio Plot with cleaned labels
  output$odds_ratio_plot <- renderPlot({
    # Get coefficients and exponentiate them for odds ratios
    model_coefs <- broom::tidy(logistic_model(), exponentiate = TRUE, conf.int = TRUE) %>%
      arrange(estimate)
    
    # Ensure intercept is included with proper label
    model_coefs$term <- clean_labels(model_coefs$term)
    
    # Create the plot, ensuring intercept is included
    ggplot(model_coefs, aes(x = term, y = estimate, ymin = conf.low, ymax = conf.high)) +
      geom_pointrange(color = "darkred") +
      geom_hline(yintercept = 1, linetype = "dashed") +
      coord_flip() +
      labs(title = "Odds Ratios for Fire Resistance",
           x = "Categories", y = "Odds Ratio") +
      theme_minimal() +
      theme(axis.text.y = element_text(size = 10))  # Adjust label size for readability
  })
  
  # --------------
  # Fire Resistance Calculator (Binary Logistic Regression)
  # --------------
  
  pred_result <- eventReactive(input$predict_btn, {
    # Build a new data frame for prediction; convert height and root depth from inches back to cm
    new_df <- tibble(
      moisture_use = factor(input$moisture_use_calc, levels = levels(plant_df$moisture_use)),
      growth_period = factor(input$growth_period_calc, levels = levels(plant_df$growth_period)),
      height = as.numeric(input$height_calc) * 2.54,
      root_depth = as.numeric(input$root_depth_calc) * 2.54,
      growth_habit = factor(input$growth_habit_calc, levels = levels(plant_df$growth_habit))
    ) %>% 
      as.data.frame()  # Explicitly convert to data.frame
    
    # Obtain predicted probabilities from the fitted model
    prob_preds <- predict(log_fit2, new_data = new_df, type = "prob")
    overall <- ifelse(prob_preds$.pred_Yes >= 0.5, "Yes", "No")
    
    # Create a formatted results table
    result <- tibble(
      `Fire Resistant?` = overall,
      `Probability (Yes)` = percent(prob_preds$.pred_Yes),
      `Probability (No)` = percent(prob_preds$.pred_No)
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

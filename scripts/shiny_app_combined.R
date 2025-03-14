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
library(ggmap)  # For basemaps
library(sf)     # For spatial data handling
library(tigris) # For county boundaries
library(dplyr)
library(tidygeocoder)
library(leaflet)
library(here)

# Simulated dataset for fire resilience prediction
generate_data <- function(n = 200) {
  data.frame(
    characteristic1 = runif(n, 0, 10),
    characteristic2 = runif(n, 0, 10),
    characteristic3 = runif(n, 0, 10),
    characteristic4 = runif(n, 0, 10),
    characteristic5 = runif(n, 0, 10),
    fire_resilient = sample(c(0, 1), n, replace = TRUE)
  )
}

data <- generate_data()

# Placeholder dataset for fire-wise plant filtering
plant_data_clean <- data.frame(
  Plant = paste("Plant", 1:50),
  h_w = sample(c("Tree", "Shrub", "Bush"), 50, replace = TRUE),
  Pollinators = sample(c("Birds", "Bees", "Butterflies"), 50, replace = TRUE),
  FireResilience = sample(c("Low", "Medium", "High"), 50, replace = TRUE),
  NurseryAvailable = sample(c(TRUE, FALSE), 50, replace = TRUE)
)

# Create a data frame with nursery names
nurseries <- data.frame(name = c("Theodore Payne Nursery, Los Angeles", 
                                 "Hahamongna Native Plant Nursery, Los Angeles", 
                                 "Artemisia Nursery, Los Angeles", 
                                 "Matilija Nursery, Moorpark",
                                 "Plant Material, Los Angeles",
                                 "Lincoln Avenue Nursery, Pasadena",
                                 "El Nativo Growers, Azusa",
                                 "Bellefontaine Nursery, Pasadena"))

# Geocode nursery addresses using OpenStreetMap (OSM)
nurseries <- nurseries |>
  geocode(address = name, method = "osm")

# Manually add missing coordinates
nurseries <- nurseries %>%
  mutate(
    lat = ifelse(is.na(lat), case_when(
      name == "Matilija Nursery, Moorpark" ~ 34.3184,
      name == "Plant Material, Los Angeles" ~ 34.0985,
      name == "Lincoln Avenue Nursery, Pasadena" ~ 34.1651,
      name == "El Nativo Growers, Azusa" ~ 34.1309,
      TRUE ~ NA_real_
    ), lat),
    long = ifelse(is.na(long), case_when(
      name == "Matilija Nursery, Moorpark" ~ -118.8811,
      name == "Plant Material, Los Angeles" ~ -118.2350,
      name == "Lincoln Avenue Nursery, Pasadena" ~ -118.1356,
      name == "El Nativo Growers, Azusa" ~ -117.9115,
      TRUE ~ NA_real_
    ), long)
  )

# Save nursery data as a CSV
write.csv(nurseries, here("data", "nurseries_geocoded.csv"), row.names = FALSE)

# Define UI
ui <- fluidPage(
  titlePanel("Fire Wise Gardens & Resilience Prediction"),
  
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = "input.tabs == 'Plant Selection'",
        selectInput("h_w", "Select Plant Height Category:",
                    choices = c("Tree", "Shrub", "Bush")),
        checkboxGroupInput("wildlife", "Select Pollinator Interactions:",
                           choices = c("Birds", "Bees", "Butterflies")),
        selectInput("water", "Select Fire Resilience Level:",
                    choices = c("Low", "Medium", "High")),
        checkboxInput("nursery_availability", "Show only plants available in LA nurseries", FALSE)
      ),
      
      conditionalPanel(
        condition = "input.tabs == 'Exploratory Data Analysis' || input.tabs == 'Model Training & Comparison'",
        selectInput("predictors", "Select Predictors:", 
                    choices = names(data)[1:5], multiple = TRUE, selected = names(data)[1:3]),
        actionButton("run_model", "Run Model", class = "btn-primary")
      )
    ),
    
    mainPanel(
      tabsetPanel(id = "tabs",
                  tabPanel("Plant Selection",
                           tableOutput("filtered_table")),
                  
                  tabPanel("Exploratory Data Analysis",
                           plotOutput("histogram"),
                           plotOutput("boxplot")),
                  
                  tabPanel("Model Training & Comparison",
                           verbatimTextOutput("model_summary"),
                           DTOutput("model_table"),
                           verbatimTextOutput("best_model")),
                  
                  tabPanel("Nursery Map",
                           leafletOutput("nursery_map"))
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  
  # Reactive expression for filtering plant data
  filtered_data <- reactive({
    data <- plant_data_clean
    
    if (input$h_w != "") {
      data <- subset(data, h_w == input$h_w)
    }
    
    if (length(input$wildlife) > 0) {
      data <- subset(data, Pollinators %in% input$wildlife)
    }
    
    if (input$water != "") {
      data <- subset(data, FireResilience == input$water)
    }
    
    if (input$nursery_availability) {
      data <- subset(data, NurseryAvailable == TRUE)
    }
    
    return(data)
  })
  
  # Render the filtered data table
  output$filtered_table <- renderTable({
    filtered_data()
  })
  
  # Reactive selection for EDA and modeling
  selected_data <- reactive({
    req(input$predictors)
    data[, c(input$predictors, "fire_resilient")]
  })
  
  # Histogram
  output$histogram <- renderPlot({
    ggplot(data, aes_string(x = input$predictors[1])) +
      geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
      labs(title = "Histogram of Selected Characteristic")
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    ggplot(data, aes(x = as.factor(fire_resilient), y = data[[input$predictors[1]]])) +
      geom_boxplot(fill = "red", alpha = 0.7) +
      labs(title = "Boxplot of Selected Characteristic by Fire Resilience")
  })
  
  # Train models when button is clicked
  model_results <- eventReactive(input$run_model, {
    set.seed(123)
    train_control <- trainControl(method = "cv", number = 10)
    models <- list(
      logistic = train(fire_resilient ~ ., data = selected_data(), method = "glm", 
                       family = "binomial", trControl = train_control),
      rf = train(fire_resilient ~ ., data = selected_data(), method = "rf", 
                 trControl = train_control),
      svm = train(fire_resilient ~ ., data = selected_data(), method = "svmRadial", 
                  trControl = train_control)
    )
    models
  })
  
  # Model summary
  output$model_summary <- renderPrint({
    req(model_results())
    summary(model_results()$logistic$finalModel)
  })
  
  # Model comparison table
  output$model_table <- renderDT({
    req(model_results())
    res <- data.frame(
      Model = names(model_results()),
      Accuracy = sapply(model_results(), function(m) max(m$results$Accuracy))
    )
    datatable(res)
  })
  
  # Display best model
  output$best_model <- renderPrint({
    req(model_results())
    best_model <- names(which.max(sapply(model_results(), function(m) max(m$results$Accuracy))))
    paste("Best Model: ", best_model)
  })
  
  # Render Leaflet nursery map
  output$nursery_map <- renderLeaflet({
    leaflet(nurseries) %>%
      addProviderTiles(providers$OpenStreetMap) %>%
      addMarkers(lng = ~long, lat = ~lat, popup = ~name)
  })
}

# Run the application 
shinyApp(ui, server)
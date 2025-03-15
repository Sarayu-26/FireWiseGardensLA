
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
library(caret)
library(DT)
library(plantr)
library(sp)
library(rvest)
library(dplyr)
library(stringr)
library(purrr)
library(here)

#generate plant characteristic dataset
oak_ex <- ptr_search(common_name = "white sagebrush", county = "Los Angeles", native = TRUE)

#not working, looks like plantr is outdated

#extract symbol names into a dataframe
ca_plant_search <- read.csv(here("data", "ca_plant_search.csv"))

#make me a code that reads html from this html, read the code from the structure of the site
#look for specific characteristics within the 

#create function for a loop or purr that adds by the symbol





#rvest scraping script
# 
# # Function to scrape plant characteristics from USDA PLANTS Database
# scrape_plant_data <- function(plant_symbol) {
#   base_url <- "https://plants.usda.gov/plant-profile/"
#   end_url <- "/characteristics.html"
#   plant_url <- paste0(base_url, plant_symbol, end_url)
#   
#   # Try fetching the webpage
  # page <- tryCatch({
  #   read_html(plant_url)
  # }, error = function(e) {
  #   message(paste("Error fetching data for:", plant_symbol))
  #   return(NULL)
  # })
  # 
  # # If page couldn't be loaded, return NA for all characteristics
  # if (is.null(page)) {
  #   return(data.frame(
  #     Plant_Symbol = plant_symbol,
  #     Growth_Period = NA, Moisture_Use = NA, Drought_Tolerance = NA, 
  #     Root_Depth = NA, Planting_Density = NA, Height = NA, Fire_Resistance = NA,
  #     stringsAsFactors = FALSE
  #   ))
  # }
  
  
#   
#   # Extract table data
#   table_nodes <- read_html(plant_url) %>% html_nodes("div#characteristics") 
#   if (length(table_nodes) == 0) {
#     message(paste("No table found for:", plant_symbol))
#     return(NULL)
#   }
#   
#   # Convert table to a dataframe
#   plant_table <- table_nodes %>% html_table(fill = TRUE) %>% .[[1]]
#   
#   # Extract specific variables from the table
#   extract_value <- function(label) {
#     row <- plant_table %>% filter(str_detect(X1, label))
#     if (nrow(row) > 0) return(row$X2[1]) else return(NA)
#   }
#   
#   data.frame(
#     Plant_Symbol = plant_symbol,
#     Growth_Period = extract_value("Active Growth Period"),
#     Moisture_Use = extract_value("Moisture Use"), #on table
#     Drought_Tolerance = extract_value("Drought Tolerance"), #on table
#     Root_Depth = extract_value("Root Depth, Minimum (inches)"), #on table
#     Temperature_Minimum = extract_value("Temperature, Minimum (°F)"), #on table
#     Planting_Density = extract_value("Planting Density per Acre, Minimum"), #on table
#     Height = extract_value("Height, Mature (feet)"), #on table
#     Fire_Resistance = extract_value("Fire Resistant"), #on table
#     Fire_Tolerance = extract_value("Fire Tolerance"), #on table
#     Precipitation_Minimum = extract_value("Precipitation, Minimum"), #on table
#     stringsAsFactors = FALSE
#   )
# }
# 
# 
# 
# # Example list of plant symbols to scrape
# plant_symbols <- c("ARLU")  # Replace with your plant symbols
# 
# # Scrape data for all plants in the list
# plant_data_list <- lapply(plant_symbols, scrape_plant_data)
# 
# # Combine results into a single dataframe
# plant_data_df <- bind_rows(plant_data_list)
# 
# # Print the scraped data
# print(plant_data_df)
# 


#load in plant data
plant_data_full <- read.csv(here("data", "ca_plants_clean_chrctr.csv"))

summary(plant_data_full)

#give me the number of yes and nos from the fire_resistance variable in 
######## Graveyard
#edit make ready for the app

# # Simulated dataset for fire resilience prediction
# generate_data <- function(n = 200) {
#   data.frame(
#     characteristic1 = runif(n, 0, 10),
#     characteristic2 = runif(n, 0, 10),
#     characteristic3 = runif(n, 0, 10),
#     characteristic4 = runif(n, 0, 10),
#     characteristic5 = runif(n, 0, 10),
#     fire_resistanceilient = sample(c(0, 1), n, replace = TRUE)
#   )
# }
# 
# data <- generate_data()

# Placeholder dataset for fire-wise plant filtering
# plant_data <- data.frame(
#   Plant = paste("Plant", 1:50),
#   h_w = sample(c("Tree", "Shrub", "Bush"), 50, replace = TRUE),
#   Pollinators = sample(c("Birds", "Bees", "Butterflies"), 50, replace = TRUE),
#   FireResilience = sample(c("Low", "Medium", "High"), 50, replace = TRUE),
#   NurseryAvailable = sample(c(TRUE, FALSE), 50, replace = TRUE)
# )

plant_data_full <- read.csv(here("data", "ca_plants_clean_chrctr.csv"), stringsAsFactors = FALSE)

print(head(plant_data_full))

str(plant_data_full)

summary(plant_data_full)

#change to numeric
plant_data_full$height <- as.numeric(plant_data_full$height)
plant_data_full$planting_density <- as.numeric(plant_data_full$planting_density)
plant_data_full$root_depth <- as.numeric(plant_data_full$root_depth)

print(str(plant_data_full))
print(head(plant_data_full))


# Reactive selection for EDA and modeling
selected_data <- reactive({
  req(input$predictors)
  filtered_data()[, c(input$predictors, "FireResistance")]  # Update column name if necessary
})

# Histogram
output$histogram <- renderPlot({
  ggplot(filtered_data(), aes_string(x = input$predictors[1])) +
    geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
    labs(title = "Histogram of Selected Characteristic")
})

# Boxplot
# output$boxplot <- renderPlot({
#   ggplot(filtered_data(), aes(x = as.factor(fire_resilience), y = .data[[input$predictors[1]]])) +
#     geom_boxplot(fill = "red", alpha = 0.7) +
#     labs(title = "Boxplot of Selected Characteristic by Fire Resilience")
# })

# Train models when button is clicked
# model_results <- eventReactive(input$run_model, {
#   set.seed(123)
#   train_control <- trainControl(method = "cv", number = 10)
#   models <- list(
#     logistic = train(FireResistance ~ ., data = selected_data(), method = "glm", 
#                      family = "binomial", trControl = train_control),
#     rf = train(FireResistance ~ ., data = selected_data(), method = "rf", 
#                trControl = train_control),
#     svm = train(FireResistance ~ ., data = selected_data(), method = "svmRadial", 
#                 trControl = train_control)
#   )
#   models
# })

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



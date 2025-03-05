
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

#generate plant characteristic dataset
oak_ex <- ptr_search(common_name = "white sagebrush", county = "Los Angeles", native = TRUE)

#not working, looks like plantr is outdated

#rvest scraping script

# Function to scrape plant characteristics from USDA PLANTS Database
scrape_plant_data <- function(plant_symbol) {
  base_url <- "https://plants.usda.gov/home/plantProfile?symbol="
  plant_url <- paste0(base_url, plant_symbol)
  
  # Try fetching the webpage
  page <- tryCatch({
    read_html(plant_url)
  }, error = function(e) {
    message(paste("Error fetching data for:", plant_symbol))
    return(NULL)
  })
  
  # If page couldn't be loaded, return NA for all characteristics
  if (is.null(page)) {
    return(data.frame(
      Plant_Symbol = plant_symbol,
      Growth_Period = NA, Moisture_Use = NA, Drought_Tolerance = NA, 
      Root_Depth = NA, Planting_Density = NA, Height = NA, Fire_Resistance = NA,
      stringsAsFactors = FALSE
    ))
  }
  
  # Extract table data
  table_nodes <- page %>% html_nodes("table") 
  if (length(table_nodes) == 0) {
    message(paste("No table found for:", plant_symbol))
    return(NULL)
  }
  
  # Convert table to a dataframe
  plant_table <- table_nodes %>% html_table(fill = TRUE) %>% .[[1]]
  
  # Extract specific variables from the table
  extract_value <- function(label) {
    row <- plant_table %>% filter(str_detect(X1, label))
    if (nrow(row) > 0) return(row$X2[1]) else return(NA)
  }
  
  data.frame(
    Plant_Symbol = plant_symbol,
    Growth_Period = extract_value("Active Growth Period"),
    Moisture_Use = extract_value("Moisture Use"), #on table
    Drought_Tolerance = extract_value("Drought Tolerance"), #on table
    Root_Depth = extract_value("Root Depth, Minimum (inches)"), #on table
    Temperature_Minimum = extract_value("Temperature, Minimum (°F)"), #on table
    Planting_Density = extract_value("Planting Density per Acre, Minimum"), #on table
    Height = extract_value("Height, Mature (feet)"), #on table
    Fire_Resistance = extract_value("Fire Resistant"), #on table
    Fire_Tolerance = extract_value("Fire Tolerance"), #on table
    Precipitation_Minimum = extract_value("Precipitation, Minimum"), #on table
    stringsAsFactors = FALSE
  )
}

# Example list of plant symbols to scrape
plant_symbols <- c("ARLU")  # Replace with your plant symbols

# Scrape data for all plants in the list
plant_data_list <- lapply(plant_symbols, scrape_plant_data)

# Combine results into a single dataframe
plant_data_df <- bind_rows(plant_data_list)

# Print the scraped data
print(plant_data_df)




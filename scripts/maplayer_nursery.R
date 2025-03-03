library(ggmap)  # For basemaps
library(sf)     # For spatial data handling
library(tigris) # For county boundaries
library(dplyr)
library(tidygeocoder)
library(leaflet)# For data manipulation

# Create a data frame with the names of the nurseries
nurseries <- data.frame(name = c("Theodore Payne Nursery, Los Angeles", 
                                 "Hahamongna Native Plant Nursery, Los Angeles", 
                                 "Artemisia Nursery, Los Angeles"))

# Geocode the address using OpenStreetMap (OSM) API
nurseries <- geocode(nurseries, address = "name", method = "osm")

# View the result with latitude and longitude
print(nurseries)
  # Uses OpenStreetMap API
nursery_map <- leaflet(nurseries) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addMarkers(lng = ~long, lat = ~lat, popup = ~name)  # Add markers with popups

# Print the map
nursery_map

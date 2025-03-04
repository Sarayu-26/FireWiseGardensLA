library(ggmap)  # For basemaps
library(sf)     # For spatial data handling
library(tigris) # For county boundaries
library(dplyr)
library(tidygeocoder)
library(leaflet)# For data manipulation

# Create a data frame with the name of the nursery
nurseries <- data.frame(name = c("Theodore Payne Nursery, Los Angeles", 
                                 "Hahamongna Native Plant Nursery,Los Angeles", 
                                 "Artemisia Nursery, Los Angeles",
))

# Geocode the address using OpenStreetMap (OSM) API
nurseries <- geocode(nurseries, address = name, method = "osm")

# View the result with latitude and longitude
print(nurseries)


nurseries <- data.frame(name = "native plant nursery Los Angeles")
nurseries <- geocode(nurseries, address = name, method = "osm")  # Uses OpenStreetMap API
library(ggmap)  # For basemaps
library(sf)     # For spatial data handling
library(tigris) # For county boundaries
library(dplyr)
library(tidygeocoder)
library(leaflet)

# Create a data frame with the names of the nurseries
nurseries <- data.frame(name = c("Theodore Payne Nursery, Los Angeles", 
                                 "Hahamongna Native Plant Nursery, Los Angeles", 
                                 "Artemisia Nursery, Los Angeles", 
                                 "Matilija Nursery, Moorpark",
                                 "Plant Material, Los Angeles",
                                 "Lincoln Avenue Nursery, Pasadena",
                                 "El Nativo Growers, Azusa",
                                 "Bellefontaine Nursery, Pasadena"))

# Geocode the address using OpenStreetMap (OSM) API
nurseries <- nurseries|>
  geocode(address = name, method = "osm")

nurseries <- nurseries %>%
  mutate(
    lat = ifelse(is.na(lat), case_when(
      name == "Matilija Nursery, Moorpark" ~ 34.3184,
      name == "Plant Material, Los Angeles" ~ 34.0985,
      name == "Lincoln Avenue Nursery, Pasadena" ~ 34.1651,
      name == "El Nativo Growers, Azusa" ~ 34.1309,
      TRUE ~ NA_real_  # Keeps other NAs unchanged
    ), lat),
    long = ifelse(is.na(long), case_when(
      name == "Matilija Nursery, Moorpark" ~ -118.8811,
      name == "Plant Material, Los Angeles" ~ -118.2350,
      name == "Lincoln Avenue Nursery, Pasadena" ~ -118.1356,
      name == "El Nativo Growers, Azusa" ~ -117.9115,
      TRUE ~ NA_real_
    ), long)
  )
#save nurseries data frame as csv 
write.csv(nurseries, here("data", "nurseries_geocoded.csv"), row.names = FALSE)

#generate leaflet map of nurseries
leaflet(nurseries)|>
  addProviderTiles(providers$OpenStreetMap) %>%  # Add default OpenStreetMap tiles
  addMarkers(lng = ~long, lat = ~lat, popup = ~name)  # Add markers with popups



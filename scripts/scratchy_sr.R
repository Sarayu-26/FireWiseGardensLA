
library(here)
species_by_nursery <- read.csv(here("data", "species_by_nursery.csv"))
species_by_nursery$website <- URLencode(species_by_nursery$website)
species_by_nursery$website <- gsub('"', "&quot;", species_by_nursery$website)
species_by_nursery$website <- as.character(species_by_nursery$website)  # Ensure it is a character vector
species_by_nursery$website <- stringr::str_trim(species_by_nursery$website)  # Remove leading/trailing spaces

# Check for non-ASCII characters and remove them
species_by_nursery$website <- iconv(species_by_nursery$website, to = "ASCII", sub = "")

website<-(species_by_nursery$website)
website


observe({
  selected_nursery <- filtered_nursery()
  print(selected_nursery)  # Check the filtered data
  
  leafletProxy("nursery_map") %>%
    clearMarkers() %>%
    clearShapes() %>%
    addMarkers(data = selected_nursery, 
               lng = ~long, lat = ~lat, 
               popup = ~paste(name))  # Simplified popup to just show the name
})

nursery_websites <- c(
  "Theodore Payne Nursery, Los Angeles" = "https://theodorepayne.org/plants-and-seeds/nursery/",
  "Artemisia Nursery, Los Angeles" = "https://artemisianursery.com/",
  "Matilija Nursery, Moorpark" = "https://www.matilijanursery.com/",
  "El Nativo Growers, Azusa" = "https://elnativogrowers.com/",
  "Plant Material, Los Angeles" = "https://plant-material.com/",
  "Lincoln Avenue Nursery" = "https://lincolnavenuenursery.com/"
  
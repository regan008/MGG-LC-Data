library(dplyr)
library(leaflet)

combined_MGG_GG

# Filter by specific city and state
Portland_data <- combined_MGG_GG %>%
  filter(city == "Portland", state == "OR")

# Remove duplicate locations (if any)
Portland_data_unique <- Portland_data %>%
  distinct(lat, lon, .keep_all = TRUE)

# Create a leaflet map
Portland_map <- leaflet(data = Portland_data_unique) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    ~lon, ~lat,   # Longitude and latitude for the circles
    popup = ~title,  # Display location name on click
    radius = 8,  # Size of the circle
    color = "blue",  # Circle color
    fillColor = "lightblue",
    fillOpacity = 0.7
  )

#view map
Portland_map
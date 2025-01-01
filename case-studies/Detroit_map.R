library(dplyr)
library(leaflet)

combined_MGG_GG

# Filter by specific city and state
Detroit_data <- combined_MGG_GG %>%
  filter(city == "Detroit", state == "MI")

# Remove duplicate locations (if any)
Detroit_data_unique <- Detroit_data %>%
  distinct(lat, lon, .keep_all = TRUE)

# Create a leaflet map
Detroit_map <- leaflet(data = Detroit_data_unique) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    ~lon, ~lat,   # Longitude and latitude for the circles
    popup = ~title,  # Display location name on click
    radius = 8,  # Size of the circle
    color = "green",  # Circle color
    fillColor = "lightgreen",
    fillOpacity = 0.7
  )

#view map
Detroit_map

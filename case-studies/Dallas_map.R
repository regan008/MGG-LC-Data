library(dplyr)
library(leaflet)

combined_MGG_GG

# Filter by specific city and state
Dallas_data <- combined_MGG_GG %>%
  filter(city == "Dallas", state == "TX")

# Remove duplicate locations (if any)
Dallas_data_unique <- Portland_data %>%
  distinct(lat, lon, .keep_all = TRUE)

# Create a leaflet map
Dallas_map <- leaflet(data = Dallas_data_unique) %>%
  addTiles() %>%  # Add default OpenStreetMap tiles
  addCircleMarkers(
    ~lon, ~lat,   # Longitude and latitude for the circles
    popup = ~title,  # Display location name on click
    radius = 8,  # Size of the circle
    color = "red",  # Circle color
    fillColor = "pink",
    fillOpacity = 0.7
  )

#view map
Dallas_map
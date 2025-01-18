library(shiny)
library(leaflet)
library(dplyr)
library(DT)

# Load the dataset
case_study_locations <- read.csv("~/MGG-LC-Data/case-studies/case_study_locations.csv")

# Define a custom palette
custom_colors <- c(
  "Damron's Guide" = "#82e0aa",
  "Gaia's Guide" = "#a569bd"
)

color_palette <- colorFactor(
  palette = custom_colors,
  domain = case_study_locations$publication
)

# Define the UI
ui <- fluidPage(
  titlePanel("Case Study Maps"),
  fluidRow(
    column(6, leafletOutput("map1", height = "400px")),
    column(6, leafletOutput("map2", height = "400px"))
  ),
  fluidRow(
    column(6, leafletOutput("map3", height = "400px")),
    column(6, leafletOutput("map4", height = "400px"))
  )
)

# Define the server logic
server <- function(input, output, session) {
  # Map 1: Full dataset
  output$map1 <- renderLeaflet({
    leaflet(data = case_study_locations) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        color = ~color_palette(publication),
        fillColor = ~color_palette(publication),
        fillOpacity = 0.7,
        radius = 8,
        popup = ~paste(
          "<b>Title:</b>", title,
          "<br><b>Type:</b>", type,
          "<br><b>Address:</b>", address,
          "<br><b>Publication:</b>", publication
        )
      )
  })
  
  # Map 2: Filtered for "Damron's Guide"
  output$map2 <- renderLeaflet({
    leaflet(data = filter(case_study_locations, publication == "Damron's Guide")) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        color = ~color_palette(publication),
        fillColor = ~color_palette(publication),
        fillOpacity = 0.7,
        radius = 8,
        popup = ~paste(
          "<b>Title:</b>", title,
          "<br><b>Type:</b>", type,
          "<br><b>Address:</b>", address,
          "<br><b>Publication:</b>", publication
        )
      )
  })
  
  # Map 3: Filtered for "Gaia's Guide"
  output$map3 <- renderLeaflet({
    leaflet(data = filter(case_study_locations, publication == "Gaia's Guide")) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        color = ~color_palette(publication),
        fillColor = ~color_palette(publication),
        fillOpacity = 0.7,
        radius = 8,
        popup = ~paste(
          "<b>Title:</b>", title,
          "<br><b>Type:</b>", type,
          "<br><b>Address:</b>", address,
          "<br><b>Publication:</b>", publication
        )
      )
  })
  
  # Map 4: Portland
  output$map4 <- renderLeaflet({
    leaflet(data =filter(case_study_locations, city == "Portland" & state == "Oregon")) %>%
      addTiles() %>%
      addCircleMarkers(
        ~lon, ~lat,
        color = ~color_palette(publication),
        fillColor = ~color_palette(publication),
        fillOpacity = 0.7,
        radius = 8,
        popup = ~paste(
          "<b>Title:</b>", title,
          "<br><b>Type:</b>", type,
          "<br><b>Address:</b>", address,
          "<br><b>Publication:</b>", publication
        )
      )
  })
}

# Run the application
shinyApp(ui = ui, server = server)


library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(plotly)
library(maps)
library(DT)

function(input, output, session) {
  
  # Reactive expression for filtered data
  filteredData <- reactive({
    req(mgg.gg.data) # Ensure mgg.gg.data is available
    data <- mgg.gg.data
    
    # data <- separate(data, geocode.value, into = c("city", "state", "country"), sep = ",")
    
    if (input$noncontiguous == FALSE) {
      data <- data %>% filter(!grepl("HI|AK|VI", geocode.value))
    }
    
    if (input$gg == FALSE) {
      data <- data %>% filter(publication != "Gaia's Guide")
    }
    
    if (input$mgg == FALSE) {
      data <- data %>% filter(publication != "Bob Damron's Address Book")
    }
    
    data
  })
  
  filteredTable <- reactive({
    req(full.data)
    
    tbldata <- full.data
    
    tbldata <- tbldata %>% filter(input$cityvalue == geocode.value)
    
    tbldata <- tbldata %>% select(-city, -state, -country, -notes, -lon, -lat, -geoAddress)
    
    tbldata
  })
  
  ## Interactive Map ###########################################
  
  # Create the map
  output$map <- renderPlotly({
    usa <- map_data("state")
    p <- ggplot() +
      geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
      geom_point(data = filteredData(), aes(x = lon, y = lat, size = relative.percentage, color = publication, text = paste("Publication:", publication, "<br>Count:", count, "<br>Geocode Value:", geocode.value)), alpha = 0.5, showlegend = FALSE)
    p <- ggplotly(p, tooltip = "text")
    p <- layout(p, showlegend = FALSE)
    p
  })
  observe({
    p <- ggplot() +
      geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
      geom_point(data = filteredData(), aes(x = lon, y = lat, size = relative.percentage, color = publication, text = paste("Publication:", publication, "<br>Count:", count, "<br>Geocode Value:", geocode.value)), alpha = 0.5) +
      
      scale_color_manual(values = c("Gaia's Guide" = "#0F8554", "Bob Damron's Address Book" = "#6F4070")) + # Manually set colors
       theme(
        axis.text.x = element_blank(), axis.text.y = element_blank(), # Remove axis labels
        axis.ticks = element_blank(), # Remove axis ticks
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()
      ) + # Remove gridlines
      labs(title = paste("Gaia's Guide & Damron's Guide: ", unique_years_string)) # Add the dynamically created title at the top of the map
    
    output$map <- renderPlotly({
      ggplotly(p, tooltip = "text") %>% layout(height = 600)  # Set the height of the plot area
    })
    
    output$dtable <- renderDT({
      filteredTable()
    })
  })
  

observeEvent(event_data("plotly_selected", source = "map"), {
  # Get the selected points from the map
  selected_points <- event_data("plotly_selected", source = "map")

  # Check if there are any selected points
  if (!is.null(selected_points)) {
    # Extract the necessary data from the selected points
    selected_data <- selected_points$x

    # Filter the data frame based on the selected data
    filtered_data <- mgg.gg.data[mgg.gg.data$geocode.value %in% selected_data, ]

    # Update the table with the filtered data frame
    output$dtable <- renderDT({filtered_data})
  }
})
})
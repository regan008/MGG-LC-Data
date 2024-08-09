library(tidyverse)
library(ggmap)
library(maps)
library(plotly)
library(htmlwidgets)

# Load the dataset
data <- read.csv("final-output-data/relative-location-data-all-years.csv")
head(data)

# Pivot the data
pivot_data <- data %>%
  select(publication, geocode.value, relative.percentage, lat, lon) %>%
  pivot_wider(names_from = publication, values_from = relative.percentage) %>%
  rename(damron_percent = `Bob Damron's Address Book`) %>%
  rename(gaia_percent = `Gaia's Guide`)

# Calculate the ratio for both directions and handle infinite/NA values
pivot_data <- pivot_data %>%
  mutate(
    damron_ratio = `damron_percent` / `gaia_percent`,
    gaia_ratio = `gaia_percent` / `damron_percent`,
    ratio = ifelse(!is.na(damron_ratio) & damron_ratio > 1, damron_ratio, 
                   ifelse(!is.na(gaia_ratio) & gaia_ratio > 1, 1 / gaia_ratio, NA))
  )

# Replace infinite or NA values resulting from division by zero
pivot_data$ratio[is.infinite(pivot_data$ratio) | is.na(pivot_data$ratio)] <- NA

# Optional: Apply log transformation for better interpretability
pivot_data <- pivot_data %>%
  mutate(log_ratio = log(ratio))

# Write pivot_data to a CSV file in the final-output-data subfolder
write.csv(pivot_data, file = "differentials/differentials.csv", row.names = FALSE)


### Doing Analysis of major differentials

relative_cutoff <- 0.08 #setting minimum cutoff value for relative_percentage in either pub
log_ratio_cutoff <- 0.5 #setting minimum cutoff value for log_ratio

frequent_places <- pivot_data %>%
    filter(`damron_percent` >= relative_cutoff | `gaia_percent` >= relative_cutoff) %>%
    filter(!is.na(log_ratio)) %>%
    arrange(desc(log_ratio))
  
outlier_places <- frequent_places %>%
    filter(abs(log_ratio) > log_ratio_cutoff) %>%
    mutate(geocode.value = sub(", United States$", "", geocode.value))

write.csv(outlier_places, file = "differentials/outlier_places.csv", row.names = FALSE)

# Create a lollipop graph to visualize the biggest location differentials
lollipop <- ggplot(outlier_places, aes(x = reorder(geocode.value, log_ratio), y = log_ratio, color = log_ratio > 0)) +
  geom_segment(aes(x = reorder(geocode.value, log_ratio), xend = reorder(geocode.value, log_ratio), y = 0, yend = log_ratio), linewidth = 1) +
  geom_point(size = 4) +
  geom_text(aes(label = ifelse(log_ratio > 0, sprintf("%.2f", damron_ratio), sprintf("%.2f", gaia_ratio)), 
                hjust = ifelse(log_ratio > 0, -1.5, 2)), size = 4) +  # Adjust hjust based on log_ratio
  coord_flip(clip = "off") +  # Flip coordinates for better readability and disable clipping
  scale_color_manual(values = c("blue", "red"), labels = c("Gaia", "Damron"), name = "Ratio skewed towards:") +  # Add legend
  labs(title = "Biggest location differentials between publications",
       subtitle = paste0("Calculated as a ratio of relative frequencies bewteen pubs", 
                         "\nShown on a log scale, but label is the raw ratio (ex. 10x as frequent in one pub)",
                         "\nOnly showing places that meet following criteria:",
                         "\n--Relative percentage in either publication >= ", relative_cutoff, 
                         "\n--Log Ratio value >= ", log_ratio_cutoff),
       caption = "Red = higher ratio towards Damron. Blue = higher ratio towards Gaia.",
       x = "geocode.value",
       y = "Log Ratio") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1),
        plot.margin = margin(10, 50, 10, 50, "pt"))  # Increase space on all sides of the plot
lollipop
ggsave("differentials/outliers_chart.png", plot = lollipop, width = 9, height = 14, dpi = 300, bg = "white")



# Plot the map with the updated size mapping
# Create a new column for size based on the log_ratio condition
outlier_places <- outlier_places %>%
  mutate(size_value = ifelse(log_ratio < 0, gaia_percent, damron_percent))
usa <- map_data("state")
p <- ggplot() +
  geom_polygon(data = usa, aes(x = long, y = lat, group = group), fill = "gray90", color = "white") +
  geom_point(data = outlier_places, aes(x = lon, y = lat, size = size_value, color = log_ratio, text = geocode.value), alpha = 0.95) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, name = "Log Ratio") +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.ticks = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(title = "Top Outliers", subtitle = "Blue = tilted towards Gaia, Red = tilted towards Damron")
# Convert ggplot to plotly for interactive features
ggplotly(p)

interactive_map <- ggplotly(p)
saveWidget(interactive_map, "images/outliers_map.html")

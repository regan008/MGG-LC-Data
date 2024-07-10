# Install necessary packages if not already installed
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")

# Load libraries
library(sf)
library(ggplot2)
library(dplyr)

# Define the paths to the shapefile and population data CSV
shapefile_path <- "~/Documents/MGG-LC-Data/simple_US_county_1980/simple_US_county_1980.shp"
population_data_path <- "~/Documents/MGG-LC-Data/nhgis0001_csv/nhgis0001_ds116_1980_county.csv"

# Read the files
counties <- st_read(shapefile_path)
population_data <- read.csv(population_data_path)

# join data
counties <- left_join(counties, population_data, by = "GISJOIN")

# Subset the dataframe to include only the year 1980
population_data_1980 <- counties %>% filter(YEAR == 1980)

# map data
ggplot(data = population_data_1980) +
  geom_sf(aes(fill = C6W001)) +
  scale_fill_viridis_c(option = "viridis", name = "Population") +
  theme_minimal() +
  labs(title = "County-level Population Map",
       subtitle = "Total Population by County",
       caption = "Source: NHGIS")
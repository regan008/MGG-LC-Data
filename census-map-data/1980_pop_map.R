install.packages("readr")
install.packages("sf")
install.packages("ipumsr")
install.packages("RColorBrewer")
install.packages("ggspatial")
library(ggspatial)
library(RColorBrewer)
library(readr) #you may have to install it using `install.packages()`. 
library(sf)
library(ipumsr)
library(tidyverse)



# Change these filepaths to the filepaths of your downloaded extract
nhgis_csv_file <- "~/Documents/GitHub/MGG-LC-Data/census-map-data/nhgis-1980-county-data/nhgis0001_ds116_1980_county.csv"
nhgis_shp_file <- "~/Documents/GitHub/MGG-LC-Data/census-map-data/US_county_1980_conflated.zip"

#load the shape file and then the data file into read_nhgis_sf
nhgis_shp <- read_ipums_sf(
  shape_file = nhgis_shp_file, bind_multiple = TRUE
)
nhgis_data <- read_nhgis(nhgis_csv_file)

#Use the ipums join file to join both the data and shape file together.
nhgis <- ipums_shape_full_join(nhgis_data, nhgis_shp, by = "GISJOIN")

#filter nhgis so that the map focuses on the 48 contiguous states. 
nhgis <- nhgis %>% filter(STATE != "Alaska Territory" & STATE != "Hawaii Territory" & STATE != "Alaska")

#plot 
ggplot(data = nhgis, aes(fill = C6W001)) +
  geom_sf() 



##adding population scale

total_population <- sum(nhgis$C6W001)
scaled_data <- nhgis %>%
  mutate(percentage = (nhgis$C6W001 / total_population) * 100)

#joining new scaled data with shape files
nhgis_scaled <- st_join(scaled_data, nhgis_shp, by = "GISJOIN")

#color info for plot
my_palette <- colorRampPalette(rev(brewer.pal(9, "Spectral")))

#new plot with pop scale
ggplot(data = nhgis_scaled) +
  geom_sf(aes(fill = percentage)) +
  scale_fill_gradientn(colors = my_palette(100), name = "Population (%)") +
  annotation_scale(location = "bl", width_hint = 0.5) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  labs(title = "Percentage of Total Population by County, 1980",
       caption = "NHGIS") +
  theme_minimal()
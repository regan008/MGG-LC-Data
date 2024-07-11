library(readr) #you may have to install it using `install.packages()`. 
library(sf)
library(ipumsr)
library(tidyverse)



# Change these filepaths to the filepaths of your downloaded extract
nhgis_csv_file <- "census-map-data/nhgis-1980-county-data/nhgis0001_ds116_1980_county.csv"
nhgis_shp_file <- "census-map-data/US_county_1980_conflated.zip"

#load the shape file and then the data file into read_nhgis_sf
nhgis_shp <- read_ipums_sf(
  shape_file = nhgis_shp_file, bind_multiple = TRUE
)
nhgis_data <- read_nhgis(nhgis_csv_file)

#Use the ipums join file to join both the data and shape file together.
nhgis <- ipums_shape_full_join(nhgis_data, nhgis_shp, by = "GISJOIN")

#filter nhgis so that the map focuses on the 48 contiguous states. 
nhgis <- nhgis %>% filter(STATE != "Alaska Territory" & STATE != "Hawaii Territory")

#plot 
ggplot(data = nhgis, aes(fill = C6W001)) +
  geom_sf() 

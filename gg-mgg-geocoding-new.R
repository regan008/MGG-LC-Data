library(tidyverse)
library(forcats)
library(ggmap)
library(ggplot2)

### Read in Datasets
columns <- c("title", "description", "type", "city", "state", "country", "publication", "year", "notes")
all.data <- data.frame()

## Read in Gaia's Guide data
# Get a list of files in the GG-Data subfolder that match the "gg-XXXX.csv" pattern
files <- list.files(path = "GG-Data", pattern = "gg-\\d{4}\\.csv$", full.names = FALSE)
years <- gsub("gg-(\\d{4})\\.csv", "\\1", files) # Extract the year from each filename
years <- as.numeric(years) # Convert the years to numeric
years <- unique(years) # Get a list of unique years
# Loop through each unique year in the subfolder to open them
for (year in years) {
  gg.filename <- paste("GG-Data/gg-", year, ".csv", sep = "")
  print(paste("Reading in ", gg.filename, sep = ""))
  #load data and add a publication and country column to match the other datasets
  gg.data <- read.csv(gg.filename, header = TRUE)
  gg.data$publication <- "Gaia's Guide"
  gg.data$country <- "United States"
  gg.data <- select(gg.data, intersect(columns, names(gg.data)))
  all.data <- rbind(all.data, gg.data)
}

## Read in MGG Data, filter it by year and add to all.data
mgg.data <- readRDS("MGG-Data/mgg-data-cleaned.rds")
mgg.data <- mgg.data %>% rename(year = Year)
mgg.data <- select(mgg.data, intersect(columns, names(mgg.data)))
mgg.data <- mgg.data %>% filter(year %in% completed_years)
all.data <- rbind(all.data, mgg.data)

### Data Cleaning and Processing

# Trim whitespace from all columns in all.data
all.data <- all.data %>%
  mutate(across(everything(), ~str_trim(.)))

# Get a unique list of cities to clean up
unique_cities_cleaning <- all.data %>%
  distinct(city, state, country) %>%
  arrange(city, state, country) # Arrange to make manual checking easier
write.csv(unique_cities_cleaning, "unique-cities-cleaning.csv", row.names = FALSE)

## NOTE: I created a new file based on unique_cities_cleaning.csv called unique_cities_replacements.csv and then manually went through and created a new column to clean up typos in the city name

# opening up the lookup csv file that has typos and their fixes, then seeing if there are any new values that need to be added to the lookup file
existing_replacements <- read.csv("unique-cities-replacements.csv", header = TRUE)
new_replacements <- unique_cities_cleaning %>%
  anti_join(existing_replacements, by = c("city", "state", "country")) %>%
  mutate(new.city = city) %>%
  select(city, new.city, everything())
write.csv(new_replacements, "unique-cities-replacements-new.csv", row.names = FALSE)

# adding any new values to the end of the existing replacements dataframe and generating a new CSV file (can scroll to end to see new values that might need to be manually cleaned)
all_replacements <- rbind(existing_replacements, new_replacements)
write.csv(all_replacements, "unique-cities-replacements.csv", row.names=FALSE)

# Join replacements with all.data and replace city values with the corrected city values 
all.data.cleaned <- all.data %>%
  left_join(all_replacements, by = c("city", "state", "country")) %>%
  mutate(city = if_else(!is.na(new.city), new.city, city)) %>%
  select(-new.city)

### GEOCODING

# getting google API key and registering with the service
getGoogleAPI <- function() {
  google_key <- readline(prompt="Please enter your Google API key: ")
  print(google_key)
  register_google(key = google_key)
}
getGoogleAPI()

# Create a geocode value column (city, state, country)
all.data.cleaned$geocode.value <- paste(all.data.cleaned$city, ", ", all.data.cleaned$state, ", ", all.data.cleaned$country, sep="")

# Generate a unique list of values you want geocoded
unique_geocode_values <- unique(all.data.cleaned$geocode.value)

# Load existing CSV file of unique locations that have already been geocoded with their lat/lon info
existing_geocoded_lookup <- read.csv("unique-locations-geocoded.csv", stringsAsFactors = FALSE)

# Create dataframes of unique locations that have already been geocoded and unique lcoations that need to be geocoded
unique.locations.all <- data.frame(geocode.value = unique_geocode_values)
unique.locations.all <- unique.locations.all %>%
  left_join(existing_geocoded_lookup, by = "geocode.value")
unique.locations.already.geocoded <- unique.locations.all %>% 
  filter(!is.na(lon))
unique.locations.to.geocode <- unique.locations.all %>% 
  filter(is.na(lon))
#unique.locations.to.geocode <- unique.locations.to.geocode %>% sample_n(5)
print(paste(length(unique.locations.to.geocode$geocode.value), " entries unmatched in unique values. Will now be geocoded.", sep = ""))

#geocoding function
for(i in 1:nrow(unique.locations.to.geocode)) {
  result <- tryCatch(geocode(unique.locations.to.geocode$geocode.value[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  unique.locations.to.geocode$lon[i] <- as.numeric(result[1])
  unique.locations.to.geocode$lat[i] <- as.numeric(result[2])
  unique.locations.to.geocode$geoAddress[i] <- as.character(result[3])
  print(paste("Result: ", toString(result)))
}

# Output a list of locations that failed to geocode
failed_geocode <- unique.locations.to.geocode %>% filter(is.na(lon))
write.csv(failed_geocode, "failed-geocode.csv", row.names = FALSE)

# Take succesffully newly geocoded locations and append them to the dataframe of already geocoded lookup locations
successful_geocode <- unique.locations.to.geocode %>% filter(!is.na(lon))
new_entries <- anti_join(successful_geocode, existing_geocoded_lookup, by = "geocode.value")
updated_existing_geocoded_lookup <- bind_rows(existing_geocoded_lookup, new_entries)
write.csv(updated_existing_geocoded_lookup, "unique-locations-geocoded.csv", row.names = FALSE)

# Merge the geocoded locations with the entire dataframe of all location records
all.data.cleaned.geocoded <- left_join(all.data.cleaned, updated_existing_geocoded_lookup, by = "geocode.value")
output_folder <- "final-output-data"
write.csv(all.data.cleaned.geocoded, file.path(output_folder, "all-data-cleaned-geocoded.csv"), row.names = FALSE)

### RELATIVE DATA CALCULATIONS

## Exporting a csv file that contains relative data for locations in each publication on year by year basis
relative.data.by.year <- function(){
  all.data <- read.csv(file = file.path(output_folder, "all-data-cleaned-geocoded.csv"))
  #calculating relative values of locations on a year by year basis
  total.per.year <- all.data %>% group_by(publication, year) %>% summarize(pub.count = n())
  total.per.loc.byyear <- all.data %>% group_by(publication, year, geocode.value, lon, lat) %>% summarize(count = n())
  relative.count <- full_join(total.per.year, total.per.loc.byyear)
  relative.count <- relative.count %>% mutate(relative.percentage = count/pub.count * 100)
  write.csv(relative.count, file=file.path(output_folder, "relative-location-data-by-year.csv"), row.names = FALSE) 
}
relative.data.by.year()

## Exporting a csv file that contains relative data for locations in each publication, with all years aggregated together
relative.data.all.years <- function(){
  all.data <- read.csv(file = file.path(output_folder, "all-data-cleaned-geocoded.csv"))
  #calculating relative values of locations on a year by year basis
  total <- all.data %>% group_by(publication) %>% summarize(pub.count = n())
  total.per.loc <- all.data %>% group_by(publication, geocode.value, lon, lat) %>% summarize(count = n())
  relative.count <- full_join(total, total.per.loc)
  relative.count <- relative.count %>% mutate(relative.percentage = count/pub.count * 100)
  write.csv(relative.count, file=file.path(output_folder, "relative-location-data-all-years.csv"), row.names = FALSE) 
}
relative.data.all.years()

## Ranked Locations by Year
ranked.locations.by.year <- function(){
  relative.count <- read.csv(file = file.path(output_folder, "relative-location-data-by-year.csv"))
  rank <- relative.count %>%  group_by(year, publication) %>% mutate(rank = rank(-count, ties.method = 'min'))
  write.csv(rank, file=file.path(output_folder, "ranked-locations-by-year.csv"))
}
ranked.locations.by.year()

## Ranked Locations for All Years

ranked.locations.all.years <- function(){
  relative.count <- read.csv(file = file.path(output_folder, "relative-location-data-all-years.csv"))
  rank <- relative.count %>%  group_by(publication) %>% mutate(rank = rank(-count, ties.method = 'min'))
  write.csv(rank, file.path(output_folder, "ranked-locations-all-years.csv"))
}
ranked.locations.all.years()

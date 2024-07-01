library(tidyverse)
library(forcats)
library(ggmap)
library(ggplot2)

### Read in Datasets

## Initialize an empty dataframe to store all data
empty.df <- data.frame(
  title = character(),
  description = character(),
  type = character(),
  city = character(),
  state = character(),
  country = character(),
  publication = character(),
  year = numeric(),
  notes = character(),
  stringsAsFactors = FALSE # This ensures that string data does not get converted to factors
)
columns <- colnames(empty.df)
years <- c(1981, 1983)

## Read in Gaia's Guide data
load_gg_data <- function(df, columns, years) {
  # Get a list of files in the GG-Data subfolder that match the "gg-XXXX.csv" pattern
  #files <- list.files(path = "GG-Data", pattern = "gg-\\d{4}\\.csv$", full.names = FALSE)
  #years <- gsub("gg-(\\d{4})\\.csv", "\\1", files) # Extract the year from each filename
  #years <- as.numeric(years) # Convert the years to numeric
  #years <- unique(years) # Get a list of unique years

  # Loop through each unique year in the subfolder to open them
  for (year in years) {
    gg.filename <- file.path("GG-Data", paste("gg-", year, ".csv", sep = ""))
    print(paste("Reading in ", gg.filename, sep = ""))
    #load data and add a publication and country column to match the other datasets
    gg.data <- read.csv(gg.filename, header = TRUE)
    gg.data$publication <- "Gaia's Guide"
    gg.data$country <- "United States"
    gg.data <- select(gg.data, intersect(columns, names(gg.data)))
    df <- rbind(df, gg.data)
  }
  return(df)
}

## Read in MGG Data, filter it by year to match the years in the Gaia's Guide data
load_mgg_data<- function(columns){
  mgg.data <- readRDS(file.path("MGG-Data", "mgg-data-cleaned.rds"))
  mgg.data <- mgg.data %>% rename(year = Year)
  mgg.data <- select(mgg.data, intersect(columns, names(mgg.data)))
  return(mgg.data)
}

## call all data loading functions
gg.data <- load_gg_data(empty.df, columns, years)
mgg.data<-load_mgg_data(columns) %>%
  filter(year %in% years)
all.data <- rbind(gg.data, mgg.data)
write.csv(all.data, file.path("data-processing-files", "all-data-precleaned.csv"), row.names = FALSE)

### Data Cleaning and Processing

# Trim whitespace from all columns in all.data
trim_all_columns <- function(data) {
  data %>%
    mutate(across(everything(), ~str_trim(.)))
}

# get a list of unique locations from all the data
generate_unique_cities_list <- function(data, output_folder) {
  unique_cities <- data %>%
    distinct(city, state, country) %>%
    arrange(city, state, country)
  write.csv(unique_cities, file.path(output_folder, "unique-cities-cleaning.csv"), row.names = FALSE)
  return(unique_cities)
}

# use a lookup file of replacements for cleaning up location names - ex. standardizing spellings, fixing typos, etc.
# the lookup file was manually checked with the second column being the place to manually fix any errors in first column
# last step is to create a csv of any NEW unique locations that don't exist in the lookup file, then also to add these the end of the replacements file - these will need to be manually checked
read_and_merge_replacements <- function(unique_cities, output_folder) {
  existing_replacements <- read.csv(file.path(output_folder, "unique-cities-replacements.csv"), header = TRUE)
  new_replacements <- unique_cities %>%
    anti_join(existing_replacements, by = c("city", "state", "country")) %>%
    mutate(new.city = city) %>%
    select(city, new.city, everything())
  all_replacements <- rbind(existing_replacements, new_replacements)
  write.csv(new_replacements, file.path(output_folder, "unique-cities-replacements-new.csv"), row.names = FALSE)
  write.csv(all_replacements, file.path(output_folder, "unique-cities-replacements.csv"), row.names = FALSE)
  return(all_replacements)
}
# take the lookup data with replacement names for cities and apply them to the main data
apply_replacements <- function(data, replacements) {
  data.cleaned <- data %>%
    left_join(replacements, by = c("city", "state", "country")) %>%
    mutate(city = if_else(!is.na(new.city), new.city, city)) %>%
    select(-new.city)
  return(data.cleaned)
  write.csv(data.cleaned, file.path(data_cleaning_folder, "all-data-cleaned.csv"), row.names = FALSE)
}

## call all data cleaning functions
data_cleaning_folder <- "data-processing-files"
all.data <- read.csv(file.path(data_cleaning_folder, "all-data-precleaned.csv"))
all.data <- trim_all_columns(all.data)
unique_cities <- generate_unique_cities_list(all.data, data_cleaning_folder)
replacements <- read_and_merge_replacements(unique_cities, data_cleaning_folder)
all.data.cleaned <- apply_replacements(all.data, replacements)

### GEOCODING

# getting google API key and registering with the service
getGoogleAPI <- function() {
  google_key <- readline(prompt="Please enter your Google API key: ")
  print(google_key)
  register_google(key = google_key)
}
getGoogleAPI()

## Geocoding function

prep_geocode <- function(data, geocoding_folder) {
  data$geocode.value <- paste(data$city, ", ", data$state, ", ", data$country, sep="") #create a column for geocoding
  unique_geocode_values <- unique(data$geocode.value) # Generate a unique list of values you want geocoded
  existing_geocoded_lookup <- read.csv(file.path(geocoding_folder, "unique-locations-geocoded.csv"), stringsAsFactors = FALSE)
  # Create dataframes of unique locations that have already been geocoded and unique lcoations that need to be geocoded
  unique.locations.all <- data.frame(geocode.value = unique_geocode_values)
  unique.locations.all <- unique.locations.all %>%
    left_join(existing_geocoded_lookup, by = "geocode.value")
  unique.locations.already.geocoded <- unique.locations.all %>% 
    filter(!is.na(lon))
  unique.locations.to.geocode <- unique.locations.all %>% 
    filter(is.na(lon))
  #unique.locations.to.geocode <- unique.locations.to.geocode %>% sample_n(5)
  print(paste(length(unique.locations.to.geocode$geocode.value), " entries unmatched in unique values that need to be geocoded.", sep = ""))
  return(unique.locations.to.geocode)
}

geocoding_function <- function(unique.locations.to.geocode, geocoding_folder) {
  for(i in 1:nrow(unique.locations.to.geocode)) {
  result <- tryCatch(geocode(unique.locations.to.geocode$geocode.value[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  unique.locations.to.geocode$lon[i] <- as.numeric(result[1])
  unique.locations.to.geocode$lat[i] <- as.numeric(result[2])
  unique.locations.to.geocode$geoAddress[i] <- as.character(result[3])
  print(paste("Result: ", toString(result)))
  }
  failed_geocode <- unique.locations.to.geocode %>% filter(is.na(lon))
  write.csv(failed_geocode, file.path(geocoding_folder, "failed-geocode.csv"), row.names = FALSE)
  return(unique.locations.to.geocode)
}

merge_geocode <- function(unique.locations.to.geocode, all.data.cleaned) {
  # Take succesffully newly geocoded locations and append them to the dataframe of already geocoded lookup locations
  successful_geocode <- unique.locations.to.geocode %>% filter(!is.na(lon))
  existing_geocoded_lookup <- read.csv(file.path(geocoding_folder, "unique-locations-geocoded.csv"), stringsAsFactors = FALSE)
  new_entries <- anti_join(successful_geocode, existing_geocoded_lookup, by = "geocode.value")
  updated_existing_geocoded_lookup <- bind_rows(existing_geocoded_lookup, new_entries)
  write.csv(updated_existing_geocoded_lookup, "unique-locations-geocoded.csv", row.names = FALSE)

  # Merge the geocoded locations with the entire dataframe of all location records
  all.data.cleaned$geocode.value <- paste(all.data.cleaned$city, ", ", all.data.cleaned$state, ", ", all.data.cleaned$country, sep="") #create a column for geocoding
  all.data.cleaned.geocoded <- left_join(all.data.cleaned, updated_existing_geocoded_lookup, by = "geocode.value")
  output_folder <- "final-output-data"
  write.csv(all.data.cleaned.geocoded, file.path(output_folder, "all-data-cleaned-geocoded.csv"), row.names = FALSE)
  return(all.data.cleaned.geocoded)
}

# call the geocoding functions
all.data.cleaned <- read.csv(file.path(data_cleaning_folder, "all-data-cleaned.csv"))
geocoding_folder <- "geocoding-files"
unique.locations.to.geocode <- prep_geocode(all.data.cleaned, geocoding_folder)
unique.locations.to.geocode <- geocoding_function(unique.locations.to.geocode, geocoding_folder)
all.data.cleaned.geocoded <- merge_geocode(unique.locations.to.geocode, all.data.cleaned)

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

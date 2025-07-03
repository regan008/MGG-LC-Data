library(tidyverse)
library(forcats)
library(ggmap)
library(ggplot2)

empty.df <- data.frame(
  record_id = character(),
  title = character(),
  description = character(),
  type = character(),
  address = character(),
  city = character(),
  state = character(),
  publication = character(),
  year = numeric(),
  notes = character(),
  amenityfeatures = character(),
  stars = character(),
  star.type = character(),
  full.address = character(),
  status = character(),
  geoAddress = character(),
  lat = numeric(),
  lon = numeric(),
  stringsAsFactors = FALSE # This ensures that string data does not get converted to factors
)
columns <- colnames(empty.df)

## Read in Gaia's Guide data
years <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989, 1991) ## CHANGE THIS TO ADD MORE YEARS AS NEEDED DEPENDING ON WHICH DATA IS COMPLETE
load_gg_data <- function(df, columns, years) {
  for (year in years) {
    gg.filename <- file.path("GG-Data", paste("gg-", year, ".csv", sep = ""))
    print(paste("Reading in ", gg.filename, sep = ""))
    gg.data <- read.csv(gg.filename, header = TRUE)
    gg.data$publication <- "Gaia's Guide"
    gg.data$year <- year

    # Add missing columns as NA
    missing_cols <- setdiff(columns, colnames(gg.data))
    for (col in missing_cols) {
      gg.data[[col]] <- NA
    }

    # Reorder columns to match empty.df
    gg.data <- gg.data[, columns]

    # Append to df
    df <- rbind(df, gg.data)
  }
  return(df)
}

## Read in MGG Data and process columns
load_mgg_data <- function(columns) {
  mgg.data <- readRDS(file.path("MGG-Data", "mgg-data.rds"))
  mgg.data <- mgg.data %>%
    rename(year = Year, address = streetaddress)
    mgg.data$publication <- "Bob Damron's Address Book"
  # Add missing columns as NA
  missing_cols <- setdiff(columns, colnames(mgg.data))
  for (col in missing_cols) {
    mgg.data[[col]] <- NA
  }
  # Reorder columns to match empty.df
  mgg.data <- mgg.data[, columns]
  return(mgg.data)
}

## Function to generate unique IDs for all records
generate_record_ids <- function(data, prefix_column, year_column, padding = 5) {
  # Define mapping for publication shorthands
  pub_map <- c("Bob Damron's Address Book" = "d", "Gaia's Guide" = "g")
  # Make a copy of the data
  result_data <- data
  # Get unique combinations of source and year
  source_year_combos <- unique(data[, c(prefix_column, year_column)])
  # For each source-year combination, generate sequential IDs
  for (i in 1:nrow(source_year_combos)) {
    source <- source_year_combos[i, prefix_column]
    year <- source_year_combos[i, year_column]
    # Map source to shorthand
    source_short <- pub_map[as.character(source)]
    # Find all rows matching this source and year
    matching_indices <- which(data[[prefix_column]] == source & 
                             data[[year_column]] == year)
    # Generate sequential numbers with padding
    seq_numbers <- sprintf(paste0("%0", padding, "d"), 1:length(matching_indices))
    # Create the unique IDs
    result_data$record_id[matching_indices] <- paste0(source_short, "-", year, "-", seq_numbers)
  }
  return(result_data)
}

#Geocoding here?

### DATA CLEANING AND GEOCODING PIPELINE

# Trim whitespace from all columns in data
trim_all_columns <- function(data) {
  data %>%
    mutate(across(everything(), ~ str_trim(.)))
}

# Generate a list of unique locations from all the data
generate_unique_cities_list <- function(data, output_folder) {
  unique_cities <- data %>%
    distinct(city, state) %>%
    arrange(city, state)
  write.csv(unique_cities, file.path(output_folder, "unique-cities-cleaning.csv"), row.names = FALSE)
  return(unique_cities)
}

# Use a lookup file of replacements for cleaning up location names
# The lookup file should be manually checked with the second column being the place to manually fix any errors in first column
# This function creates a csv of any NEW unique locations that don't exist in the lookup file
read_and_merge_replacements <- function(unique_cities, output_folder) {
  # Check if replacements file exists, create empty one if not
  replacements_file <- file.path(output_folder, "unique-cities-replacements.csv")
  if (file.exists(replacements_file)) {
    existing_replacements <- read.csv(replacements_file, header = TRUE)
  } else {
    existing_replacements <- data.frame(
      city = character(),
      new.city = character(),
      state = character(),
      stringsAsFactors = FALSE
    )
  }
  
  new_replacements <- unique_cities %>%
    anti_join(existing_replacements, by = c("city", "state")) %>%
    mutate(new.city = city) %>%
    select(city, new.city, everything())
  
  all_replacements <- rbind(existing_replacements, new_replacements)
  write.csv(new_replacements, file.path(output_folder, "unique-cities-replacements-new.csv"), row.names = FALSE)
  write.csv(all_replacements, file.path(output_folder, "unique-cities-replacements.csv"), row.names = FALSE)
  return(all_replacements)
}

# Apply the lookup data with replacement names for cities to the main data
apply_replacements <- function(data, replacements) {
  data.cleaned <- data %>%
    left_join(replacements, by = c("city", "state")) %>%
    mutate(city = if_else(!is.na(new.city), new.city, city)) %>%
    select(-new.city)
  return(data.cleaned)
}

### GEOCODING FUNCTIONS

# Get Google API key and register with the service
getGoogleAPI <- function() {
  google_key <- readline(prompt = "Please enter your Google API key: ")
  print(google_key)
  register_google(key = google_key)
}

# Check for existing geocoded locations and prepare a list of unique locations to geocode
prep_geocode <- function(data, geocoding_folder) {
  # Create geocode values based on city and state
  data$geocode.value <- ifelse(
    is.na(data$state) | data$state == "",
    NA,  # Return NA if no state (and no city)
    ifelse(
      is.na(data$city) | data$city == "" | tolower(data$city) == "statewide",
      paste(data$state, ", United States", sep = ""),  # Geocode state for statewide/blank entries
      paste(data$city, ", ", data$state, sep = "")     # Geocode city, state for specific cities
    )
  )
  
  # Filter out invalid entries before creating unique list
  valid_data <- data %>%
    filter(!is.na(state) & state != "")
  
  unique_geocode_values <- unique(valid_data$geocode.value) # Generate a unique list of values you want geocoded
  
  # Check if geocoded lookup file exists, create empty one if not
  lookup_file <- file.path(geocoding_folder, "unique-locations-geocoded.csv")
  if (file.exists(lookup_file)) {
    existing_geocoded_lookup <- read.csv(lookup_file, stringsAsFactors = FALSE)
  } else {
    existing_geocoded_lookup <- data.frame(
      geocode.value = character(),
      lon = numeric(),
      lat = numeric(),
      geoAddress = character(),
      stringsAsFactors = FALSE
    )
  }
  
  # Create dataframes of unique locations that have already been geocoded and unique locations that need to be geocoded
  unique.locations.all <- data.frame(geocode.value = unique_geocode_values)
  unique.locations.all <- unique.locations.all %>%
    left_join(existing_geocoded_lookup, by = "geocode.value")
  
  unique.locations.already.geocoded <- unique.locations.all %>%
    filter(!is.na(lon))
  
  unique.locations.to.geocode <- unique.locations.all %>%
    filter(is.na(lon))
  
  # Filter out invalid geocode values
  unique.locations.to.geocode <- unique.locations.to.geocode %>%
    filter(geocode.value != "" & geocode.value != ", " & !is.na(geocode.value))
  
  print(paste(length(unique.locations.to.geocode$geocode.value), " entries unmatched in unique values that need to be geocoded.", sep = ""))
  return(unique.locations.to.geocode)
}

# Function to geocode unique locations and update the lookup file
geocoding_function <- function(unique.locations.to.geocode, all.data.cleaned, geocoding_folder, output_folder) {
  if (nrow(unique.locations.to.geocode) == 0) {
    cat("No new locations to geocode.\n")
    # Still need to merge with existing data and create final output
    return(merge_with_existing_geocoded_data(all.data.cleaned, geocoding_folder, output_folder))
  }
  
  for (i in 1:nrow(unique.locations.to.geocode)) {
    # Skip empty or invalid geocode values
    if (is.na(unique.locations.to.geocode$geocode.value[i]) || 
        unique.locations.to.geocode$geocode.value[i] == "" ||
        unique.locations.to.geocode$geocode.value[i] == ", ") {
      cat("Skipping invalid geocode value:", unique.locations.to.geocode$geocode.value[i], "\n")
      unique.locations.to.geocode$lon[i] <- NA
      unique.locations.to.geocode$lat[i] <- NA
      unique.locations.to.geocode$geoAddress[i] <- NA
      next
    }
    
    result <- tryCatch(
      geocode(unique.locations.to.geocode$geocode.value[i], output = "latlona", source = "google"), 
      error = function(w) {
        cat("Error geocoding:", unique.locations.to.geocode$geocode.value[i], "-", w$message, "\n")
        data.frame(lon = NA, lat = NA, address = NA)
      }
    )
    
    # Check if result has the expected structure
    if (ncol(result) >= 3 && !is.na(result[1]) && !is.na(result[2])) {
      unique.locations.to.geocode$lon[i] <- as.numeric(result[1])
      unique.locations.to.geocode$lat[i] <- as.numeric(result[2])
      unique.locations.to.geocode$geoAddress[i] <- as.character(result[3])
      print(paste("Geocoded:", unique.locations.to.geocode$geocode.value[i], "->", toString(result)))
    } else {
      unique.locations.to.geocode$lon[i] <- NA
      unique.locations.to.geocode$lat[i] <- NA
      unique.locations.to.geocode$geoAddress[i] <- NA
      cat("Failed to geocode:", unique.locations.to.geocode$geocode.value[i], "\n")
    }
    
    # Add a small delay to avoid hitting API rate limits
    Sys.sleep(0.1)
  }
  
  # Handle failed geocoding attempts
  failed_geocode <- unique.locations.to.geocode %>% filter(is.na(lon))
  if (nrow(failed_geocode) > 0) {
    write.csv(failed_geocode, file.path(geocoding_folder, "failed-geocode.csv"), row.names = FALSE)
    cat("Failed to geocode", nrow(failed_geocode), "locations. Check failed-geocode.csv\n")
  }
  
  # Update the lookup file with successful geocoding results
  successful_geocode <- unique.locations.to.geocode %>% filter(!is.na(lon))
  
  lookup_file <- file.path(geocoding_folder, "unique-locations-geocoded.csv")
  if (file.exists(lookup_file)) {
    existing_geocoded_lookup <- read.csv(lookup_file, stringsAsFactors = FALSE)
  } else {
    existing_geocoded_lookup <- data.frame(
      geocode.value = character(),
      lon = numeric(),
      lat = numeric(),
      geoAddress = character(),
      stringsAsFactors = FALSE
    )
  }
  
  new_entries <- anti_join(successful_geocode, existing_geocoded_lookup, by = "geocode.value")
  updated_existing_geocoded_lookup <- bind_rows(existing_geocoded_lookup, new_entries)
  write.csv(updated_existing_geocoded_lookup, file.path(geocoding_folder, "unique-locations-geocoded.csv"), row.names = FALSE)
  
  cat("Updated unique-locations-geocoded.csv with", nrow(new_entries), "new entries\n")
  
  # Merge with main dataset and return final result
  return(merge_with_existing_geocoded_data(all.data.cleaned, geocoding_folder, output_folder))
}

# Helper function to merge geocoded data with main dataset
merge_with_existing_geocoded_data <- function(all.data.cleaned, geocoding_folder, output_folder) {
  # Use the same logic as prep_geocode to create geocode.value
  all.data.cleaned$geocode.value <- ifelse(
    is.na(all.data.cleaned$state) | all.data.cleaned$state == "",
    NA,  # Return NA if no state (and no city)
    ifelse(
      is.na(all.data.cleaned$city) | all.data.cleaned$city == "" | tolower(all.data.cleaned$city) == "statewide",
      paste(all.data.cleaned$state, ", United States", sep = ""),  # Geocode state for statewide/blank entries
      paste(all.data.cleaned$city, ", ", all.data.cleaned$state, sep = "")     # Geocode city, state for specific cities
    )
  )
  
  # Read the updated lookup file
  lookup_file <- file.path(geocoding_folder, "unique-locations-geocoded.csv")
  if (file.exists(lookup_file)) {
    existing_geocoded_lookup <- read.csv(lookup_file, stringsAsFactors = FALSE)
  } else {
    existing_geocoded_lookup <- data.frame(
      geocode.value = character(),
      lon = numeric(),
      lat = numeric(),
      geoAddress = character(),
      stringsAsFactors = FALSE
    )
  }
  
  all.data.cleaned.geocoded <- left_join(all.data.cleaned, existing_geocoded_lookup, by = "geocode.value")
  write.csv(all.data.cleaned.geocoded, file.path(output_folder, "all-data-cleaned-geocoded.csv"), row.names = FALSE)
  return(all.data.cleaned.geocoded)
}

# Generate a documentation file to list out completed years
documentation_function <- function(csvFile, markdownFile, output_folder) {
  data <- read.csv(file.path(output_folder, csvFile))
  unique_years <- data %>%
    distinct(year) %>%
    arrange(year) %>%
    pull(year)
  md_file <- file(file.path(output_folder, markdownFile), "w")
  writeLines("# Completed Years\n", md_file)
  writeLines("This document lists all the years for which data cleaning and geocoding have been completed.\n", md_file)
  for (year in unique_years) {
    writeLines(sprintf("- %s", year), md_file)
  }
  close(md_file)
}

## Function to clean up coordinates - ex. swapped lat/lon values, coerce missing or incorrect values to NA, etc.
fix_lat_lon_swaps <- function(df, lat_col = "lat", lon_col = "lon") {
  # Make a copy of the dataframe
  fixed_df <- df
  
  # Convert columns to numeric for comparison
  lat_numeric <- suppressWarnings(as.numeric(df[[lat_col]]))
  lon_numeric <- suppressWarnings(as.numeric(df[[lon_col]]))
  
  # Initialize counters
  swap_count <- 0
  
  # Check each row
  for (i in 1:nrow(df)) {
    lat_val <- lat_numeric[i]
    lon_val <- lon_numeric[i]
    
    # Skip if either value is NA
    if (is.na(lat_val) || is.na(lon_val)) {
      next
    }
    
    # Case 1: Latitude out of range but longitude in valid latitude range
    if ((lat_val < -90 || lat_val > 90) && (lon_val >= -90 && lon_val <= 90) && abs(lat_val) <= 180) {
      # Swap the values
      temp <- fixed_df[[lat_col]][i]
      fixed_df[[lat_col]][i] <- fixed_df[[lon_col]][i]
      fixed_df[[lon_col]][i] <- temp
      swap_count <- swap_count + 1
    }
    
    # Case 2: Longitude out of range but latitude in valid longitude range
    else if ((lon_val < -180 || lon_val > 180) && (lat_val >= -90 && lat_val <= 90) && abs(lon_val) <= 90) {
      # Swap the values
      temp <- fixed_df[[lat_col]][i]
      fixed_df[[lat_col]][i] <- fixed_df[[lon_col]][i]
      fixed_df[[lon_col]][i] <- temp
      swap_count <- swap_count + 1
    }
  }
  cat("Fixed", swap_count, "swapped coordinate pairs\n")
  
  # Coerce lat and lon columns to numeric
  fixed_df[[lat_col]] <- as.numeric(fixed_df[[lat_col]])
  fixed_df[[lon_col]] <- as.numeric(fixed_df[[lon_col]])
  
  # If either lat or lon is NA, set both to NA
  na_indices <- which(is.na(fixed_df[[lat_col]]) | is.na(fixed_df[[lon_col]]))
  if (length(na_indices) > 0) {
    fixed_df[[lat_col]][na_indices] <- NA
    fixed_df[[lon_col]][na_indices] <- NA
  }
  return(fixed_df)
}

## Reading in data and applying functions

gg.data <- load_gg_data(empty.df, columns, years)
mgg.data <- load_mgg_data(columns)
all.data <- rbind(gg.data, mgg.data)
all.data <- generate_record_ids(all.data, "publication", "year", padding = 5)
all.data <- fix_lat_lon_swaps(all.data)

## Write out the data
write.csv(all.data, file = "full-data-processing/full-data-processed.csv", row.names = FALSE)

### EXECUTE DATA CLEANING AND GEOCODING PIPELINE

# Set up folder paths
data_cleaning_folder <- "full-data-processing"
geocoding_folder <- "full-data-processing/geocoding-files"
output_folder <- "full-data-processing/final-output-data"

# Create directories if they don't exist
dir.create(geocoding_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)

# Step 1: Data cleaning
cat("=== STEP 1: DATA CLEANING ===\n")
all.data <- trim_all_columns(all.data)
unique_cities <- generate_unique_cities_list(all.data, data_cleaning_folder)
replacements <- read_and_merge_replacements(unique_cities, data_cleaning_folder)
all.data.cleaned <- apply_replacements(all.data, replacements)

# Write cleaned data
write.csv(all.data.cleaned, file.path(data_cleaning_folder, "all-data-cleaned.csv"), row.names = FALSE)

# Step 2: Geocoding (optional - uncomment if you want to geocode)
cat("=== STEP 2: GEOCODING ===\n")
getGoogleAPI()  # Uncomment this line when you want to geocode
unique.locations.to.geocode <- prep_geocode(all.data.cleaned, geocoding_folder)
all.data.cleaned.geocoded <- geocoding_function(unique.locations.to.geocode, all.data.cleaned, geocoding_folder, output_folder)
documentation_function("all-data-cleaned-geocoded.csv", "completed_years.md", output_folder)

## Type column processing

# Install and load the textclean package
if(!require(textclean)) install.packages("textclean")
if(!require(hunspell)) install.packages("hunspell")
library(hunspell)
library(textclean)

# Load the data for cleaning
all.data.cleaned <- all.data

# Handle null/missing values
# Replace "null" entries with NA and decide how to handle them
all.data.cleaned$type[all.data.cleaned$type == "null" | all.data.cleaned$type == "" | is.na(all.data.cleaned$type)] <- NA
cat("Found", sum(is.na(all.data.cleaned$type)), "null/missing type entries\n")

# Add new column type_clean, trim whitespace, convert to lowercase for consistent processing
all.data.cleaned$type_clean <- trimws(all.data.cleaned$type)
all.data.cleaned$type_clean <- tolower(all.data.cleaned$type_clean)

# Apply text cleaning functions to standardize the type_clean column
all.data.cleaned <- all.data.cleaned %>%
  mutate(
    # First create a clean version of the type column
    type_clean = replace_non_ascii(type_clean),      # Replace non-ASCII characters
    type_clean = replace_symbol(type_clean),         # Replace symbols with text
    type_clean = gsub("\\s+", " ", type_clean),      # Standardize spaces
    type_clean = trimws(type_clean)                  # Trim whitespace
  )

type_counts_original <- all.data.cleaned %>%
  count(type, sort = TRUE) %>%
  arrange(desc(n))

type_counts_cleaned <- all.data.cleaned %>%
  count(type_clean, sort = TRUE) %>%
  arrange(desc(n))

type_counts_more_one <- type_counts_cleaned %>%
  filter(n > 1) %>%
  arrange(type_clean)

write_csv(type_counts_more_one, file = "full-data-processing/cleaned-types-counts-more-one.csv")
write_csv(type_counts_cleaned, file = "full-data-processing/cleaned-types-counts.csv")

### PIPELINE USAGE INSTRUCTIONS

# HOW TO ADD A NEW YEAR OF DATA TO THE PIPELINE:
#
# 1. ADD NEW YEAR TO THE YEARS VECTOR (line ~25):
#    years <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989, 1991, NEW_YEAR)
#
# 2. ENSURE DATA FILES EXIST:
#    - For Gaia's Guide: Place "gg-NEW_YEAR.csv" in the "GG-Data" folder
#    - For MGG: Ensure the RDS file in "MGG-Data" contains the new year
#
# 3. RUN THE DATA CLEANING PIPELINE:
#    - The script will automatically generate "unique-cities-cleaning.csv" 
#    - Review and edit "unique-cities-replacements-new.csv" for location name corrections
#    - Copy corrected entries to "unique-cities-replacements.csv"
#
# 4. OPTIONAL: RUN GEOCODING:
#    - Uncomment the geocoding section (lines ~180-185)
#    - Provide your Google API key when prompted
#    - The script will only geocode new locations not already in the lookup file
#
# 5. OUTPUT FILES:
#    - "all-data-cleaned.csv": Cleaned data without geocoding
#    - "all-data-cleaned-geocoded.csv": Final data with coordinates (if geocoding enabled)
#    - "completed_years.md": Documentation of processed years
#
# 6. FOLDER STRUCTURE:
#    full-data-processing/
#    ├── geocoding-files/
#    │   ├── unique-locations-geocoded.csv (lookup file)
#    │   └── failed-geocode.csv (failed geocoding attempts)
#    ├── final-output-data/
#    │   ├── all-data-cleaned-geocoded.csv
#    │   └── completed_years.md
#    ├── unique-cities-cleaning.csv
#    ├── unique-cities-replacements.csv
#    └── unique-cities-replacements-new.csv
#
# NOTES:
# - The geocoding lookup file persists between runs, so you only geocode new locations
# - Failed geocoding attempts are saved for manual review
# - Location name corrections are cumulative and reusable across years

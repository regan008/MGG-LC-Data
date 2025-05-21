library(tidyverse)
library(forcats)

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
  full.address = character(),
  status = character(),
  geoAddress = character(),
  lat = numeric(),
  lon = numeric(),
  stringsAsFactors = FALSE # This ensures that string data does not get converted to factors
)
columns <- colnames(empty.df)

## Read in Gaia's Guide data
years <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989) ## CHANGE THIS TO ADD MORE YEARS AS NEEDED DEPENDING ON WHICH DATA IS COMPLETE
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

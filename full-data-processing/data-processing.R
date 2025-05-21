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
years <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989) ## CHANGE THIS TO ADD MORE YEARS AS NEEDED DEPENDING ON WHICH DATA IS COMPLETE

## Read in Gaia's Guide data
# ...existing code...

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

## Read in MGG Data, filter it by year to match the years in the Gaia's Guide data
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
gg.data <- load_gg_data(empty.df, columns, years)
mgg.data <- load_mgg_data(columns)
all.data <- rbind(gg.data, mgg.data)



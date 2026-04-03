library(tidyverse)
library(ggmap)
library(DBI)
library(RSQLite)

# ---------------------------------------------------------------------------
# SCHEMA
# ---------------------------------------------------------------------------

empty.df <- data.frame(
  record_id           = character(),
  title               = character(),
  description         = character(),
  address             = character(),
  city                = character(),
  state               = character(),
  type                = character(),
  publication         = character(),
  year                = numeric(),
  notes               = character(),
  amenityfeatures     = character(),
  stars               = character(),
  star.type           = character(),
  unclear.address     = character(),
  statewide.address   = character(),
  mentions.race       = character(),
  mentions.disability = character(),
  transcription.method = character(),
  full.address        = character(),
  status              = character(),
  geoAddress          = character(),
  lat                 = numeric(),
  lon                 = numeric(),
  stringsAsFactors = FALSE
)
columns <- colnames(empty.df)

# ---------------------------------------------------------------------------
# DATA LOADING
# ---------------------------------------------------------------------------

# Normalise Gaia's Guide column names to the canonical schema.
# Handles all known Airtable export variants across years 1975-1991.
normalize_gg_columns <- function(data) {
  n <- colnames(data)

  # --- Race / disability mention columns ---
  # Variants: "Mentions Race?", "mentions.race"
  n[n == "Mentions Race?"]        <- "mentions.race"
  n[n == "Mentions.Race."]        <- "mentions.race"
  # Variants: "Mentions Disability?", "mentions.disability", "mention.disability" (1989 typo)
  n[n == "Mentions Disability?"]  <- "mentions.disability"
  n[n == "Mentions.Disability."]  <- "mentions.disability"
  n[n == "mention.disability"]    <- "mentions.disability"

  # --- Star type ---
  # Variants: "Star Type" (1985, space not dot)
  n[n == "Star Type"]             <- "star.type"
  n[n == "Star.Type"]             <- "star.type"

  # --- Airtable artifact columns — rename to something clearly ignorable ---
  # These are dropped downstream when we select only schema columns,
  # but renaming avoids silent name collisions.
  n[n == "Last Modified"]                        <- "airtable.last.modified"
  n[n == "Last.Modified"]                        <- "airtable.last.modified"
  n[n == "Last Modified By"]                     <- "airtable.last.modified.by"
  n[n == "Last.Modified.By"]                     <- "airtable.last.modified.by"
  n[n == "Notes.2"]                              <- "airtable.notes.2"
  n[n == "Questions/Unsure Dr. R should check"]  <- "airtable.questions"
  n[n == "Start date"]                           <- "airtable.start.date"
  n[n == "Feminist Bookstores"]                  <- "airtable.feminist.bookstores"

  colnames(data) <- n
  return(data)
}

# Load all Gaia's Guide CSVs for the given years.
# Normalises column names and fills missing schema columns with NA.
load_gg_data <- function(df, columns, years) {
  for (year in years) {
    gg.filename <- file.path("GG-Data", paste0("gg-", year, ".csv"))
    cat("Reading", gg.filename, "\n")
    gg.data <- read.csv(gg.filename, header = TRUE, check.names = FALSE)
    gg.data <- normalize_gg_columns(gg.data)
    gg.data$publication <- "Gaia's Guide"
    gg.data$year <- year

    missing_cols <- setdiff(columns, colnames(gg.data))
    for (col in missing_cols) gg.data[[col]] <- NA

    gg.data <- gg.data[, columns]
    df <- rbind(df, gg.data)
  }
  return(df)
}

# Load Bob Damron data from SQLite. Types are joined and collapsed to a
# comma-separated string to match the GG schema.
load_mgg_data <- function(db_path, columns) {
  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con))

  query <- "
    SELECT
      l.unique_id          AS record_id,
      l.title,
      l.description,
      l.street_address     AS address,
      l.city,
      l.state,
      l.year,
      l.notes,
      l.full_address       AS full_address,
      l.status,
      l.geo_address        AS geoAddress,
      l.latitude           AS lat,
      l.longitude          AS lon,
      GROUP_CONCAT(lt.name, ',') AS type
    FROM locations l
    LEFT JOIN location_type_assignments lta ON l.id = lta.location_id
    LEFT JOIN location_types lt ON lta.type_id = lt.id
    GROUP BY l.id
  "

  mgg.data <- DBI::dbGetQuery(con, query)

  # Rename dot-form column (SQL can't produce dots in aliases)
  mgg.data <- mgg.data %>% rename(full.address = full_address)

  # Convert "NA" strings from the DB to actual NA
  mgg.data[mgg.data == "NA"] <- NA

  mgg.data$publication <- "Bob Damron's Address Book"

  missing_cols <- setdiff(columns, colnames(mgg.data))
  for (col in missing_cols) mgg.data[[col]] <- NA

  mgg.data <- mgg.data[, columns]
  return(mgg.data)
}

# ---------------------------------------------------------------------------
# RECORD IDs
# ---------------------------------------------------------------------------

# Generate sequential IDs in the form g-YEAR-NNNNN / d-YEAR-NNNNN.
# Only fills rows where record_id is already blank/NA (so Damron IDs from
# the DB are preserved).
generate_record_ids <- function(data, prefix_column, year_column, padding = 5) {
  pub_map <- c("Bob Damron's Address Book" = "d", "Gaia's Guide" = "g")
  result_data <- data
  source_year_combos <- unique(data[, c(prefix_column, year_column)])

  for (i in 1:nrow(source_year_combos)) {
    src  <- source_year_combos[i, prefix_column]
    yr   <- source_year_combos[i, year_column]
    short <- pub_map[as.character(src)]

    # Only generate IDs for rows that don't already have one
    matching <- which(
      data[[prefix_column]] == src &
      data[[year_column]]   == yr &
      (is.na(data$record_id) | data$record_id == "")
    )
    if (length(matching) == 0) next

    seq_nums <- sprintf(paste0("%0", padding, "d"), seq_along(matching))
    result_data$record_id[matching] <- paste0(short, "-", yr, "-", seq_nums)
  }
  return(result_data)
}

# ---------------------------------------------------------------------------
# VALIDATION
# ---------------------------------------------------------------------------

# Check required fields and warn about bad rows. Returns the data unchanged;
# stops the pipeline if critical problems are found.
validate_raw_data <- function(data) {
  cat("=== VALIDATION ===\n")
  issues <- 0

  # Missing state
  no_state <- which(is.na(data$state) | data$state == "")
  if (length(no_state) > 0) {
    cat(sprintf("WARNING: %d rows have no state value:\n", length(no_state)))
    print(data[no_state, c("record_id", "title", "city", "year", "publication")])
    issues <- issues + length(no_state)
  }

  # Non-numeric year
  bad_year <- which(is.na(suppressWarnings(as.numeric(data$year))))
  if (length(bad_year) > 0) {
    cat(sprintf("WARNING: %d rows have non-numeric year:\n", length(bad_year)))
    print(data[bad_year, c("record_id", "title", "year", "publication")])
    issues <- issues + length(bad_year)
  }

  # Duplicate record IDs
  dupe_ids <- data$record_id[duplicated(data$record_id) & !is.na(data$record_id)]
  if (length(dupe_ids) > 0) {
    cat(sprintf("ERROR: %d duplicate record_id values found:\n", length(dupe_ids)))
    print(unique(dupe_ids))
    stop("Duplicate record IDs — cannot continue. Check source data.")
  }

  cat(sprintf("Validation complete: %d rows, %d warnings, 0 blocking errors.\n",
              nrow(data), issues))
  return(data)
}

# Row-count assertion — stops pipeline if a join introduced duplicates.
assert_row_count <- function(data, expected, step_name) {
  actual <- nrow(data)
  if (actual != expected) {
    stop(sprintf(
      "Row count assertion FAILED at '%s': expected %d rows, got %d. Check for join duplication.",
      step_name, expected, actual
    ))
  }
  cat(sprintf("Row count OK at '%s': %d rows\n", step_name, actual))
}

# ---------------------------------------------------------------------------
# DATA CLEANING
# ---------------------------------------------------------------------------

# Generate a CSV of unique city/state pairs for review.
generate_unique_cities_list <- function(data, output_folder) {
  unique_cities <- data %>%
    distinct(city, state) %>%
    arrange(city, state)
  write.csv(unique_cities, file.path(output_folder, "unique-cities-cleaning.csv"), row.names = FALSE)
  return(unique_cities)
}

# Trim whitespace from every column.
trim_all_columns <- function(data) {
  data %>% mutate(across(everything(), ~ str_trim(.)))
}

# Apply city corrections from a lookup CSV (columns: city, new.city, state).
# Joins on (city, state), replaces city with new.city where they differ.
# Asserts that the join does not introduce duplicate rows.
apply_city_corrections <- function(data, lookup_file) {
  lookup <- read.csv(lookup_file, stringsAsFactors = FALSE)

  # Only rows where a real correction is present
  lookup_clean <- lookup %>%
    filter(!is.na(new.city) & new.city != "" & new.city != city)

  # Guard: duplicates in the lookup cause row duplication via left_join
  dupe_keys <- lookup_clean %>% group_by(city, state) %>% filter(n() > 1)
  if (nrow(dupe_keys) > 0) {
    cat(sprintf("WARNING: %d duplicate keys in corrections lookup — keeping first.\n",
                nrow(dupe_keys)))
    lookup_clean <- lookup_clean %>% distinct(city, state, .keep_all = TRUE)
  }

  cat(sprintf("Loaded %d active corrections from %s\n", nrow(lookup_clean), lookup_file))
  n_before <- nrow(data)

  data_corrected <- data %>%
    left_join(lookup_clean, by = c("city", "state")) %>%
    mutate(city = if_else(!is.na(new.city), new.city, city)) %>%
    select(-new.city)

  assert_row_count(data_corrected, n_before, "apply_city_corrections")

  corrections_applied <- sum(data$city != data_corrected$city, na.rm = TRUE)
  cat(sprintf("Applied corrections to %d rows\n", corrections_applied))
  return(data_corrected)
}

# ---------------------------------------------------------------------------
# GEOCODING
# ---------------------------------------------------------------------------

# Load Google API key from .env file or environment variable.
load_google_api_key <- function() {
  env_file <- ".env"
  if (file.exists(env_file)) {
    lines <- readLines(env_file, warn = FALSE)
    for (line in lines) {
      if (grepl("^GOOGLE_MAPS_API_KEY=", line)) {
        key <- sub("^GOOGLE_MAPS_API_KEY=", "", line)
        Sys.setenv(GOOGLE_MAPS_API_KEY = key)
      }
    }
  }
  google_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")
  if (google_key == "") {
    stop("Google Maps API key not found. Add GOOGLE_MAPS_API_KEY=<key> to your .env file.")
  }
  register_google(key = google_key)
  cat("Google API key loaded.\n")
}

# Identify unique locations not yet in the geocoding cache.
prep_geocode <- function(data, geocoding_folder) {
  data_to_geocode <- data %>%
    filter((is.na(lat) | lat == "" | lat == "NA") &
           (is.na(lon) | lon == "" | lon == "NA"))

  data_to_geocode$geocode.value <- ifelse(
    is.na(data_to_geocode$state) | data_to_geocode$state == "",
    NA,
    ifelse(
      is.na(data_to_geocode$city) | data_to_geocode$city == "" |
        tolower(data_to_geocode$city) == "statewide",
      paste0(data_to_geocode$state, ", United States"),
      paste0(data_to_geocode$city, ", ", data_to_geocode$state)
    )
  )

  valid_data <- data_to_geocode %>% filter(!is.na(state) & state != "")
  unique_geocode_values <- unique(valid_data$geocode.value)

  lookup_file <- file.path(geocoding_folder, "unique-locations-geocoded.csv")
  if (file.exists(lookup_file)) {
    existing <- read.csv(lookup_file, stringsAsFactors = FALSE)
  } else {
    existing <- data.frame(geocode.value = character(), lon = numeric(),
                           lat = numeric(), geoAddress = character(),
                           stringsAsFactors = FALSE)
  }

  unique.locations.all <- data.frame(geocode.value = unique_geocode_values) %>%
    left_join(existing, by = "geocode.value")

  to_geocode <- unique.locations.all %>%
    filter(is.na(lon)) %>%
    filter(geocode.value != "" & geocode.value != ", " & !is.na(geocode.value))

  cat(sprintf("%d new locations need geocoding.\n", nrow(to_geocode)))
  return(to_geocode)
}

# Geocode new locations and update the cache.
geocoding_function <- function(unique.locations.to.geocode, all.data.cleaned,
                               geocoding_folder, output_folder) {
  if (nrow(unique.locations.to.geocode) == 0) {
    cat("No new locations to geocode.\n")
    return(merge_with_existing_geocoded_data(all.data.cleaned, geocoding_folder, output_folder))
  }

  for (i in 1:nrow(unique.locations.to.geocode)) {
    val <- unique.locations.to.geocode$geocode.value[i]
    if (is.na(val) || val == "" || val == ", ") {
      unique.locations.to.geocode$lon[i] <- NA
      unique.locations.to.geocode$lat[i] <- NA
      unique.locations.to.geocode$geoAddress[i] <- NA
      next
    }

    result <- tryCatch(
      geocode(val, output = "latlona", source = "google"),
      error = function(e) {
        cat("Error geocoding:", val, "-", e$message, "\n")
        data.frame(lon = NA, lat = NA, address = NA)
      }
    )

    if (ncol(result) >= 3 && !is.na(result[1,1]) && !is.na(result[1,2])) {
      unique.locations.to.geocode$lon[i]        <- as.numeric(result[1,1])
      unique.locations.to.geocode$lat[i]        <- as.numeric(result[1,2])
      unique.locations.to.geocode$geoAddress[i] <- as.character(result[1,3])
      cat(sprintf("Geocoded: %s\n", val))
    } else {
      unique.locations.to.geocode$lon[i] <- NA
      unique.locations.to.geocode$lat[i] <- NA
      unique.locations.to.geocode$geoAddress[i] <- NA
      cat(sprintf("Failed: %s\n", val))
    }
    Sys.sleep(0.1)
  }

  failed <- unique.locations.to.geocode %>% filter(is.na(lon))
  if (nrow(failed) > 0) {
    write.csv(failed, file.path(geocoding_folder, "failed-geocode.csv"), row.names = FALSE)
    cat(sprintf("Failed to geocode %d locations. See failed-geocode.csv\n", nrow(failed)))
  }

  successful <- unique.locations.to.geocode %>% filter(!is.na(lon))
  lookup_file <- file.path(geocoding_folder, "unique-locations-geocoded.csv")
  existing <- if (file.exists(lookup_file)) {
    read.csv(lookup_file, stringsAsFactors = FALSE)
  } else {
    data.frame(geocode.value = character(), lon = numeric(), lat = numeric(),
               geoAddress = character(), stringsAsFactors = FALSE)
  }
  new_entries <- anti_join(successful, existing, by = "geocode.value")
  updated <- bind_rows(existing, new_entries)
  write.csv(updated, lookup_file, row.names = FALSE)
  cat(sprintf("Added %d new entries to geocoding cache.\n", nrow(new_entries)))

  return(merge_with_existing_geocoded_data(all.data.cleaned, geocoding_folder, output_folder))
}

# Merge the geocoding cache into the main dataset.
# If the cache has been enriched (locality, is_neighborhood columns), those
# are carried through automatically.
merge_with_existing_geocoded_data <- function(all.data.cleaned, geocoding_folder, output_folder) {
  all.data.cleaned$geocode.value <- ifelse(
    is.na(all.data.cleaned$state) | all.data.cleaned$state == "",
    NA,
    ifelse(
      is.na(all.data.cleaned$city) | all.data.cleaned$city == "" |
        tolower(all.data.cleaned$city) == "statewide",
      paste0(all.data.cleaned$state, ", United States"),
      paste0(all.data.cleaned$city, ", ", all.data.cleaned$state)
    )
  )

  lookup_file <- file.path(geocoding_folder, "unique-locations-geocoded.csv")
  cache <- if (file.exists(lookup_file)) {
    read.csv(lookup_file, stringsAsFactors = FALSE)
  } else {
    data.frame(geocode.value = character(), lon = numeric(), lat = numeric(),
               geoAddress = character(), stringsAsFactors = FALSE)
  }

  n_before <- nrow(all.data.cleaned)
  merged <- left_join(all.data.cleaned, cache, by = "geocode.value") %>%
    mutate(
      lat        = ifelse(!is.na(lat.x),        lat.x,        lat.y),
      lon        = ifelse(!is.na(lon.x),        lon.x,        lon.y),
      geoAddress = ifelse(!is.na(geoAddress.x) & geoAddress.x != "",
                          geoAddress.x, geoAddress.y)
    ) %>%
    select(-lat.x, -lat.y, -lon.x, -lon.y, -geoAddress.x, -geoAddress.y)
  assert_row_count(merged, n_before, "merge_with_existing_geocoded_data")

  write.csv(merged, file.path(output_folder, "all-data-cleaned-geocoded.csv"), row.names = FALSE)
  return(merged)
}

# Derive canonical_city and metro_city from enriched geocoding cache columns.
# Call this after merge_with_existing_geocoded_data if the cache has been
# enriched by enrich-geocode-cache.R.
apply_canonical_city <- function(data, metro_overrides_file) {
  # canonical_city: use locality from Google if available, else fall back to city
  data <- data %>%
    mutate(canonical_city = if_else(!is.na(locality), locality, city))

  # metro_city: join metro overrides table (PI-controlled CSV)
  if (file.exists(metro_overrides_file) &&
      file.info(metro_overrides_file)$size > 0) {
    overrides <- read.csv(metro_overrides_file, stringsAsFactors = FALSE)
    if (nrow(overrides) > 0) {
      n_before <- nrow(data)
      data <- data %>%
        left_join(overrides, by = c("locality" = "locality", "state" = "state")) %>%
        rename(metro_city = metro_city)
      assert_row_count(data, n_before, "apply_canonical_city metro join")
    } else {
      data$metro_city <- NA_character_
    }
  } else {
    data$metro_city <- NA_character_
  }

  n_neighborhood <- sum(data$is_neighborhood == TRUE, na.rm = TRUE)
  cat(sprintf("canonical_city derived: %d neighbourhoods remapped to parent city\n",
              n_neighborhood))
  return(data)
}

# ---------------------------------------------------------------------------
# COORDINATE CLEANING (vectorized)
# ---------------------------------------------------------------------------

fix_lat_lon_swaps <- function(df, lat_col = "lat", lon_col = "lon") {
  lat <- suppressWarnings(as.numeric(df[[lat_col]]))
  lon <- suppressWarnings(as.numeric(df[[lon_col]]))

  valid <- !is.na(lat) & !is.na(lon)

  # Case 1: lat out of [-90, 90] but lon in [-90, 90] and |lat| <= 180
  case1 <- valid & (lat < -90 | lat > 90) & (lon >= -90 & lon <= 90) & abs(lat) <= 180
  # Case 2: lon out of [-180, 180] but lat in [-90, 90] and |lon| <= 90
  case2 <- valid & (lon < -180 | lon > 180) & (lat >= -90 & lat <= 90) & abs(lon) <= 90

  to_swap <- case1 | case2
  tmp         <- lat[to_swap]
  lat[to_swap] <- lon[to_swap]
  lon[to_swap] <- tmp

  cat(sprintf("Fixed %d swapped coordinate pairs\n", sum(to_swap)))

  # If either is NA, set both to NA
  either_na <- is.na(lat) | is.na(lon)
  lat[either_na] <- NA
  lon[either_na] <- NA

  df[[lat_col]] <- lat
  df[[lon_col]] <- lon
  return(df)
}

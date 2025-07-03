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






####### Code below was for experimenting with spell checking, standardization, etc. Don't need to implement.

# STEP 1: Initial Setup - Extract all unique misspelled words from dataset
cat("=== STEP 1: EXTRACTING MISSPELLED WORDS ===\n")

extract_misspelled_words <- function(df, text_column) {
  cat("Analyzing", nrow(df), "entries for misspellings...\n")
  
  # Extract all text from the specified column
  all_text <- df[[text_column]]
  
  # Split into individual words, removing punctuation and converting to lowercase
  all_words <- unlist(str_split(all_text, "[\\s/&,.-]+"))
  all_words <- tolower(all_words[all_words != "" & !is.na(all_words)])
  
  # Get unique words only (more efficient)
  unique_words <- unique(all_words)
  
  # Check spelling for all unique words
  spelling_results <- hunspell_check(unique_words)
  
  # Get misspelled words
  misspelled_words <- unique_words[!spelling_results]
  
  cat("Found", length(misspelled_words), "unique misspelled words out of", length(unique_words), "total unique words\n")
  
  return(misspelled_words)
}

# Extract misspelled words from your dataset
misspelled_words <- extract_misspelled_words(all.data.cleaned, "type_clean")

# Display sample of misspelled words
cat("Sample of misspelled words found:\n")
print(head(misspelled_words, 15))

# STEP 2: Pre-populate CSV with hunspell suggestions
cat("\n=== STEP 2: CREATING CORRECTION DICTIONARY CSV ===\n")

create_correction_csv <- function(misspelled_words, filename) {
  cat("Creating correction dictionary with", length(misspelled_words), "entries...\n")
  
  # Create empty vectors to store results
  misspelled <- character()
  top_suggestion <- character()
  all_suggestions <- character()
  confidence <- character()
  notes <- character()
  
  # Process each misspelled word
  for(word in misspelled_words) {
    suggestions <- hunspell_suggest(word)[[1]]
    
    misspelled <- c(misspelled, word)
    
    # Get top suggestion (if available)
    if(length(suggestions) > 0) {
      top_suggestion <- c(top_suggestion, suggestions[1])
      # Store up to 3 suggestions for reference
      all_suggestions <- c(all_suggestions, paste(suggestions[1:min(3, length(suggestions))], collapse = "; "))
      confidence <- c(confidence, ifelse(length(suggestions) >= 3, "High", "Medium"))
    } else {
      top_suggestion <- c(top_suggestion, word)  # Keep original if no suggestions
      all_suggestions <- c(all_suggestions, "No suggestions")
      confidence <- c(confidence, "Low")
    }
    
    notes <- c(notes, "")  # Empty notes field for manual input
  }
  
  # Create correction dictionary dataframe
  correction_dict <- data.frame(
    misspelled = misspelled,
    correction = top_suggestion,
    all_suggestions = all_suggestions,
    confidence = confidence,
    notes = notes,
    stringsAsFactors = FALSE
  )
  
  # Sort by misspelled word for easier manual review
  correction_dict <- correction_dict[order(correction_dict$misspelled), ]
  
  # Save to CSV
  write_csv(correction_dict, filename)
  cat("Correction dictionary saved to:", filename, "\n")
  cat("Please review and edit the 'correction' column manually.\n")
  cat("You can also add notes in the 'notes' column for documentation.\n")
  
  return(correction_dict)
}

# Create the initial correction CSV
misspelled_filepath <- "full-data-processing/misspelled-words-correction.csv"
initial_corrections <- create_correction_csv(misspelled_words, misspelled_filepath)

corrections_lookup.df <- read_csv("full-data-processing/misspelled-words-correction-lookup.csv", show_col_types = FALSE)

sample_all.data.clean<-sample_n(all.data.cleaned, 50)
write_csv(sample_all.data.clean, "full-data-processing/sample-all-data-clean.csv")


# Function to apply spelling corrections with word boundary matching
apply_spelling_corrections <- function(data, lookup_file) {
  
  # Read the spelling correction lookup table
  corrections <- read_csv(lookup_file)
  
  # Check if the required columns exist
  if (!all(c("misspelled", "correction") %in% names(corrections))) {
    stop("Lookup file must contain 'misspelled' and 'correction' columns")
  }
  
  # Remove any NA values from the corrections table
  corrections <- corrections %>%
    filter(!is.na(misspelled) & !is.na(correction))
  
  # Create a copy of the data to work with
  data_corrected <- data
  
  # Apply each correction using word boundaries
  for (i in seq_len(nrow(corrections))) {
    misspelled_word <- corrections$misspelled[i]
    correct_word <- corrections$correction[i]
    
    # Create regex pattern with word boundaries
    # \b ensures we match complete words only
    # We also need to escape special regex characters in the misspelled word
    escaped_word <- str_escape(misspelled_word)
    pattern <- paste0("\\b", escaped_word, "\\b")
    
    # Apply the correction
    data_corrected$type_clean <- str_replace_all(
      data_corrected$type_clean, 
      regex(pattern, ignore_case = TRUE), 
      correct_word
    )
    
    # Print progress (optional)
    cat("Applied correction:", misspelled_word, "->", correct_word, "\n")
  }
  
  return(data_corrected)
}

# Apply the corrections
all.data.cleaned <- apply_spelling_corrections(
  all.data.cleaned, 
  "full-data-processing/misspelled-words-correction-lookup.csv"
)

# Check the results
type_counts_cleaned <- all.data.cleaned %>%
  count(type_clean, sort = TRUE) %>%
  arrange(desc(n))
write_csv(type_counts_cleaned, file="full-data-processing/cleaned-types-counts.csv")


# Simple function to pluralize specified words in type_clean column

library(dplyr)
library(stringr)

# Function to pluralize a list of words
pluralize_words <- function(data, words_to_pluralize) {
  
  # Create a copy to work with
  data_updated <- data
  
  # Apply pluralization to each word
  for (word in words_to_pluralize) {
    
    # Create the plural form (simple 's' addition)
    plural_word <- paste0(word, "s")
    
    # Replace using word boundaries
    pattern <- paste0("\\b", word, "\\b")
    data_updated$type_clean <- str_replace_all(
      data_updated$type_clean, 
      pattern, 
      plural_word
    )
    
    cat("Pluralized:", word, "->", plural_word, "\n")
  }
  
  return(data_updated)
}

# Define your list of words to pluralize
words_to_pluralize <- c("bookstore", "hotel", "cafe", "publication", "restaurant", "bar")

# Apply the pluralization
all.data.cleaned <- pluralize_words(all.data.cleaned, words_to_pluralize)

cat("\nPluralization complete!")

# Write out the cleaned type counts for manual review
#write.csv(type_counts_cleaned, file="full-data-processing/cleaned-types-counts.csv", row.names = FALSE)

type_counts_cleaned <- all.data.cleaned %>%
  count(type_clean, sort = TRUE) %>%
  arrange(desc(n))

## counting unique types 
type_counts <- all.data %>%
  count(type, name = "count") %>%
  arrange(type)
write.csv(type_counts, file="full-data-processing/unique-types-full-data.csv", row.names = FALSE)

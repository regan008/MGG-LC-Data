# LAYER 3: CLEANING & ENRICHMENT
#
# Reads raw-combined.csv, applies city corrections, geocodes new locations
# (from cache or Google API), normalises types, and maps types to categories.
# Writes cleaned-geocoded.csv.
#
# Run via run-pipeline.R, not directly.

if (!require(textclean)) install.packages("textclean")
library(textclean)

cat("=== LAYER 3: CLEANING & ENRICHMENT ===\n")

# --- Read input ---
all.data <- read.csv(file.path(INTERMEDIATE_FOLDER, "raw-combined.csv"),
                     stringsAsFactors = FALSE)
cat(sprintf("Read raw-combined.csv: %d rows\n", nrow(all.data)))

# --- Trim whitespace ---
all.data <- trim_all_columns(all.data)

# --- Generate unique cities list (for review / corrections workflow) ---
generate_unique_cities_list(all.data, CLEANING_FOLDER)

# --- City corrections ---
cat("=== CITY CORRECTIONS ===\n")
corrections_file <- file.path(CLEANING_FOLDER, "unique-cities-replacements.csv")
all.data.cleaned <- apply_city_corrections(all.data, corrections_file)

# --- Geocoding ---
cat("=== GEOCODING ===\n")
if (SKIP_GEOCODING) {
  cat("SKIP_GEOCODING = TRUE — merging from cache only.\n")
  all.data.cleaned.geocoded <- merge_with_existing_geocoded_data(
    all.data.cleaned, GEOCODING_FOLDER, OUTPUT_FOLDER
  )
} else {
  load_google_api_key()
  unique.locations.to.geocode <- prep_geocode(all.data.cleaned, GEOCODING_FOLDER)
  all.data.cleaned.geocoded <- geocoding_function(
    unique.locations.to.geocode, all.data.cleaned, GEOCODING_FOLDER, OUTPUT_FOLDER
  )
}
cat(sprintf("After geocoding: %d rows\n", nrow(all.data.cleaned.geocoded)))

# --- Canonical city derivation ---
# Only runs if the cache has been enriched (locality column present).
if ("locality" %in% colnames(all.data.cleaned.geocoded)) {
  cat("=== CANONICAL CITY ===\n")
  all.data.cleaned.geocoded <- apply_canonical_city(
    all.data.cleaned.geocoded,
    metro_overrides_file = file.path(CLEANING_FOLDER, "metro-overrides.csv")
  )
} else {
  cat("Note: geocoding cache not yet enriched — run enrich-geocode-cache.R to add canonical_city.\n")
}

# --- Type normalisation ---
cat("=== TYPE NORMALISATION ===\n")
type.cleaning <- all.data.cleaned.geocoded

type.cleaning$type[type.cleaning$type == "null" | type.cleaning$type == "" |
                   is.na(type.cleaning$type)] <- NA
cat(sprintf("Null/missing type entries: %d\n", sum(is.na(type.cleaning$type))))

type.cleaning$type_clean <- tolower(trimws(type.cleaning$type))
type.cleaning <- type.cleaning %>%
  mutate(
    type_clean = replace_non_ascii(type_clean),
    type_clean = replace_symbol(type_clean),
    type_clean = gsub("\\s+", " ", type_clean),
    type_clean = trimws(type_clean)
  )

# --- Type-to-category mapping ---
cat("=== TYPE-TO-CATEGORY MAPPING ===\n")
type_category_lookup <- read.csv(
  file.path(CLEANING_FOLDER, "type-category/type-category-lookup.csv"),
  stringsAsFactors = FALSE
)

n_before <- nrow(type.cleaning)
type.cleaning <- type.cleaning %>%
  left_join(type_category_lookup, by = c("type_clean" = "type"))
assert_row_count(type.cleaning, n_before, "type-category join")

unmatched <- type.cleaning %>%
  filter(!is.na(type_clean) & is.na(category)) %>%
  distinct(type_clean) %>%
  nrow()
cat(sprintf("Unmatched types (no category): %d unique values\n", unmatched))

# Write type count diagnostics
type_counts <- type.cleaning %>% count(type_clean, sort = TRUE)
write_csv(type_counts,
          file.path(OUTPUT_FOLDER, "cleaned-types-counts.csv"))
write_csv(type_counts %>% filter(n > 1),
          file.path(OUTPUT_FOLDER, "cleaned-types-counts-more-one.csv"))

# Promote type_clean → type, drop temp column
type.cleaning$type <- type.cleaning$type_clean
type.cleaning$type_clean <- NULL

# --- Export ---
full_export <- type.cleaning

write_csv(full_export,
          file.path(OUTPUT_FOLDER, "all-data-cleaned-geocoded.csv"))

filtered_by_year <- full_export %>% filter(year %in% YEARS)
write_csv(filtered_by_year,
          file.path(OUTPUT_FOLDER, "filtered-years-cleaned-geocoded.csv"))

cat(sprintf("Wrote all-data-cleaned-geocoded.csv (%d rows)\n", nrow(full_export)))
cat(sprintf("Wrote filtered-years-cleaned-geocoded.csv (%d rows)\n", nrow(filtered_by_year)))

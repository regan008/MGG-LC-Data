# LAYER 4: ANALYSIS & EXPORT
#
# Reads cleaned-geocoded.csv (in memory — no CSV re-reads mid-analysis),
# calculates relative frequencies and rankings, and writes output CSVs.
#
# Run via run-pipeline.R, not directly.

cat("=== LAYER 4: ANALYSIS & EXPORT ===\n")

# --- Read input (once) ---
full_export <- read_csv(
  file.path(OUTPUT_FOLDER, "all-data-cleaned-geocoded.csv"),
  show_col_types = FALSE
)
cat(sprintf("Read all-data-cleaned-geocoded.csv: %d rows\n", nrow(full_export)))

filtered_by_year <- full_export %>% filter(year %in% YEARS)
cat(sprintf("Filtered to target years: %d rows\n", nrow(filtered_by_year)))

# Use the filtered dataset for all analysis below
analysis_data <- filtered_by_year

# ---------------------------------------------------------------------------
# Relative frequency by year
# ---------------------------------------------------------------------------

cat("Calculating relative data by year...\n")

total_per_year <- analysis_data %>%
  group_by(publication, year) %>%
  summarize(total.locations = n(), .groups = "drop")

total_per_loc_by_year <- analysis_data %>%
  group_by(publication, year, geocode.value) %>%
  summarize(count = n(), .groups = "drop")

relative_by_year <- full_join(total_per_year, total_per_loc_by_year,
                               by = c("publication", "year")) %>%
  mutate(relative.percentage = count / total.locations * 100) %>%
  group_by(year, publication) %>%
  mutate(location_rank = dense_rank(desc(count))) %>%
  ungroup()

write.csv(relative_by_year,
          file.path(OUTPUT_FOLDER, "filtered-years-relative-location-by-year.csv"),
          row.names = FALSE)
cat(sprintf("Wrote filtered-years-relative-location-by-year.csv (%d rows)\n",
            nrow(relative_by_year)))

# ---------------------------------------------------------------------------
# Relative frequency across all years (per publication)
# ---------------------------------------------------------------------------

cat("Calculating relative data across all years...\n")

total_per_pub <- analysis_data %>%
  group_by(publication) %>%
  summarize(total.locations = n(), .groups = "drop")

total_per_loc_all <- analysis_data %>%
  group_by(publication, geocode.value) %>%
  summarize(count = n(), .groups = "drop")

relative_all_years <- full_join(total_per_pub, total_per_loc_all,
                                by = "publication") %>%
  mutate(relative.percentage = count / total.locations * 100) %>%
  select(publication, geocode.value, count, relative.percentage) %>%
  group_by(publication) %>%
  mutate(location_rank = dense_rank(desc(count))) %>%
  ungroup()

write.csv(relative_all_years,
          file.path(OUTPUT_FOLDER, "filtered-years-relative-location-by-publication-total.csv"),
          row.names = FALSE)
cat(sprintf("Wrote filtered-years-relative-location-by-publication-total.csv (%d rows)\n",
            nrow(relative_all_years)))

# ---------------------------------------------------------------------------
# Per-publication ranked lists
# ---------------------------------------------------------------------------

gg_ranks <- relative_all_years %>%
  filter(publication == "Gaia's Guide") %>%
  arrange(location_rank) %>%
  select(geocode.value, location_rank, count, relative.percentage)

bd_ranks <- relative_all_years %>%
  filter(publication == "Bob Damron's Address Book") %>%
  arrange(location_rank) %>%
  select(geocode.value, location_rank, count, relative.percentage)

write.csv(gg_ranks, file.path(OUTPUT_FOLDER, "gg-ranks.csv"), row.names = FALSE)
write.csv(bd_ranks, file.path(OUTPUT_FOLDER, "bd-ranks.csv"), row.names = FALSE)

cat("Analysis complete.\n")

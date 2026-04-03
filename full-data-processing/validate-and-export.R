# LAYER 2: VALIDATED INGEST
#
# Loads Gaia's Guide CSVs and Bob Damron data from SQLite, validates both,
# combines them, and writes raw-combined.csv.
#
# Run via run-pipeline.R, not directly.

cat("=== LAYER 2: VALIDATED INGEST ===\n")

# --- Load Gaia's Guide ---
cat("Loading Gaia's Guide data...\n")
gg.data <- load_gg_data(empty.df, columns, YEARS)
cat(sprintf("  GG rows loaded: %d\n", nrow(gg.data)))

# Generate record IDs for GG (Damron IDs come from the DB)
gg.data <- generate_record_ids(gg.data, "publication", "year", padding = 5)

# Mark transcription method — CSVs from the new Google Sheets template will
# carry their own value; pre-template CSVs default to "manual".
if (!"transcription.method" %in% colnames(gg.data) ||
    all(is.na(gg.data$transcription.method))) {
  gg.data$transcription.method <- "manual"
}

# --- Load Bob Damron from SQLite ---
cat("Loading Bob Damron data from SQLite...\n")
mgg.data <- load_mgg_data(DAMRON_DB_PATH, columns)
cat(sprintf("  Damron rows loaded: %d\n", nrow(mgg.data)))
mgg.data$transcription.method <- "database"

# Filter to the years we care about
if (!is.null(YEARS)) {
  gg.data  <- gg.data  %>% filter(year %in% YEARS)
  mgg.data <- mgg.data %>% filter(year %in% YEARS)
  cat(sprintf("  After year filter — GG: %d rows, Damron: %d rows\n",
              nrow(gg.data), nrow(mgg.data)))
}

# --- Combine ---
all.data <- rbind(gg.data, mgg.data)
cat(sprintf("Combined total: %d rows\n", nrow(all.data)))

# --- Validate ---
all.data <- validate_raw_data(all.data)

# --- Coordinate cleanup ---
all.data <- fix_lat_lon_swaps(all.data)

# --- Write output ---
raw_out <- file.path(INTERMEDIATE_FOLDER, "raw-combined.csv")
write.csv(all.data, raw_out, row.names = FALSE)
cat(sprintf("Wrote %s (%d rows)\n", raw_out, nrow(all.data)))

# UTILITY: Export GG CSVs in Canonical Format
#
# Reads each existing GG-Data/gg-YYYY.csv, normalises the column names to the
# canonical schema, and writes the result to GG-Data/canonical/gg-YYYY.csv.
#
# Run this once before uploading a year's data to Google Sheets for editing.
# The originals in GG-Data/ are never modified.
#
# Usage (from repo root):
#   Rscript full-data-processing/export-gg-canonical.R
#
# To process a single year only, edit the YEARS vector below.

library(tidyverse)

# ---------------------------------------------------------------------------
# CONFIG
# ---------------------------------------------------------------------------

YEARS      <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989, 1991)
INPUT_DIR  <- "GG-Data"
OUTPUT_DIR <- "GG-Data/canonical"

# Canonical columns in the order they should appear in the output CSV.
# This matches gg-template.csv exactly.
CANONICAL_COLS <- c(
  "title", "description", "address", "city", "state",
  "type", "stars", "star.type", "notes", "year",
  "unclear.address", "statewide.address",
  "mentions.race", "mentions.disability",
  "transcription.method"
)

# ---------------------------------------------------------------------------
# COLUMN NORMALISATION
# (duplicated here so this script runs standalone without sourcing functions.R)
# ---------------------------------------------------------------------------

normalize_gg_columns <- function(data) {
  n <- colnames(data)

  n[n == "Mentions Race?"]                       <- "mentions.race"
  n[n == "Mentions.Race."]                       <- "mentions.race"
  n[n == "Mentions Disability?"]                 <- "mentions.disability"
  n[n == "Mentions.Disability."]                 <- "mentions.disability"
  n[n == "mention.disability"]                   <- "mentions.disability"
  n[n == "Star Type"]                            <- "star.type"
  n[n == "Star.Type"]                            <- "star.type"
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

# ---------------------------------------------------------------------------
# PROCESS
# ---------------------------------------------------------------------------

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

results <- data.frame(
  year        = integer(),
  rows        = integer(),
  dropped_cols = character(),
  added_cols   = character(),
  stringsAsFactors = FALSE
)

for (yr in YEARS) {
  infile  <- file.path(INPUT_DIR,  paste0("gg-", yr, ".csv"))
  outfile <- file.path(OUTPUT_DIR, paste0("gg-", yr, ".csv"))

  if (!file.exists(infile)) {
    cat(sprintf("SKIP  %d — file not found: %s\n", yr, infile))
    next
  }

  raw  <- read.csv(infile, header = TRUE, check.names = FALSE,
                   stringsAsFactors = FALSE)
  norm <- normalize_gg_columns(raw)

  original_cols <- colnames(raw)
  norm_cols     <- colnames(norm)

  # Columns that existed in the source but are dropped (Airtable artifacts, etc.)
  dropped <- setdiff(original_cols, CANONICAL_COLS)
  dropped <- dropped[!grepl("^airtable\\.", dropped)]  # already renamed, don't double-report

  # Canonical columns missing from this year's file — will be added as blank
  missing <- setdiff(CANONICAL_COLS, norm_cols)

  # Add missing canonical columns as empty strings
  for (col in missing) {
    norm[[col]] <- ""
  }

  # Select and reorder to canonical column order
  out <- norm[, CANONICAL_COLS]

  # Ensure transcription.method is set for any row that doesn't have it
  out$transcription.method[is.na(out$transcription.method) |
                           out$transcription.method == ""] <- "manual"

  # Normalise checkbox columns to TRUE/FALSE for Google Sheets compatibility.
  # Airtable exports these as "checked" or blank; Sheets expects TRUE/FALSE.
  checkbox_cols <- c("unclear.address", "statewide.address",
                     "mentions.race", "mentions.disability")
  for (col in checkbox_cols) {
    if (col %in% colnames(out)) {
      out[[col]] <- ifelse(
        tolower(trimws(as.character(out[[col]]))) %in% c("checked", "true", "yes", "1"),
        "TRUE",
        "FALSE"
      )
    }
  }

  write.csv(out, outfile, row.names = FALSE, na = "")

  cat(sprintf("OK    %d — %d rows → %s\n", yr, nrow(out), outfile))
  if (length(dropped) > 0)
    cat(sprintf("        dropped : %s\n", paste(dropped, collapse = ", ")))
  if (length(missing) > 0)
    cat(sprintf("        added   : %s (blank)\n", paste(missing, collapse = ", ")))

  results <- rbind(results, data.frame(
    year         = yr,
    rows         = nrow(out),
    dropped_cols = paste(dropped,  collapse = "; "),
    added_cols   = paste(missing,  collapse = "; "),
    stringsAsFactors = FALSE
  ))
}

cat("\n=== Summary ===\n")
print(results, row.names = FALSE)
cat(sprintf("\nCanonical CSVs written to: %s/\n", OUTPUT_DIR))
cat("Upload those files to Google Sheets for editing.\n")
cat("When done, download each sheet as CSV and save to GG-Data/gg-YYYY.csv\n")

# MGG-LC-Data Pipeline
# Run this script from the repo root to execute the full pipeline.
#
# Usage:
#   Rscript run-pipeline.R
#   or open in RStudio and Source the file.

# ===========================================================================
# CONFIGURATION — edit here
# ===========================================================================

YEARS <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989, 1991)

# Path to the Bob Damron SQLite database (relative to repo root)
DAMRON_DB_PATH <- "mgg.db"

# Set TRUE to skip Google API calls and use only the existing geocoding cache.
# Set FALSE when you have new locations that need geocoding.
SKIP_GEOCODING <- TRUE

# Folder paths (relative to repo root — do not change unless you move files)
CLEANING_FOLDER    <- "full-data-processing"
GEOCODING_FOLDER   <- "full-data-processing/geocoding-files"
OUTPUT_FOLDER      <- "full-data-processing/final-output-data"
INTERMEDIATE_FOLDER <- "full-data-processing/intermediate"

# ===========================================================================
# SETUP
# ===========================================================================

library(tidyverse)

# Create output directories if they don't exist
dir.create(GEOCODING_FOLDER,    recursive = TRUE, showWarnings = FALSE)
dir.create(OUTPUT_FOLDER,       recursive = TRUE, showWarnings = FALSE)
dir.create(INTERMEDIATE_FOLDER, recursive = TRUE, showWarnings = FALSE)

source("full-data-processing/functions.R")

# ===========================================================================
# PIPELINE
# ===========================================================================

source("full-data-processing/validate-and-export.R")   # → intermediate/raw-combined.csv
source("full-data-processing/clean-and-geocode.R")     # → final-output-data/all-data-cleaned-geocoded.csv
source("full-data-processing/analyze-and-export.R")    # → final-output-data/*.csv

cat("\n=== PIPELINE COMPLETE ===\n")

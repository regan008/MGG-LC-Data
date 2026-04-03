# MGG-LC-Data Pipeline Overview

This document describes the data processing pipeline for the Mapping the Gay Guides project. It combines two data sources — **Gaia's Guide** (GG) and **Bob Damron's Address Book** (MGG) — cleans and geocodes the records, and produces output CSVs for analysis and visualization.

---

## How to Run

From the repo root, open `run-pipeline.R` in RStudio and click **Source**, or run from the terminal:

```bash
Rscript run-pipeline.R
```

All configuration (years, paths, geocoding toggle) lives at the top of `run-pipeline.R`. You should not need to edit any other file for a routine run.

---

## Architecture

The pipeline is split into four layers, each in its own script under `full-data-processing/`. A shared `functions.R` defines all reusable functions. The orchestrator `run-pipeline.R` sets config variables and sources each layer in order.

```
run-pipeline.R                    ← entry point, config, orchestrator
│
├── full-data-processing/
│   ├── functions.R               ← all shared function definitions
│   ├── validate-and-export.R     ← Layer 2: load + validate + combine
│   ├── clean-and-geocode.R       ← Layer 3: clean + geocode + categorize
│   ├── analyze-and-export.R      ← Layer 4: relative data + rankings
│   └── enrich-geocode-cache.R    ← one-time utility (see below)
```

Data flows like this:

```
GG CSVs (GG-Data/)          mgg.db (SQLite)
       │                          │
       └──────────┬───────────────┘
                  ▼
         validate-and-export.R
         → intermediate/raw-combined.csv
                  │
                  ▼
         clean-and-geocode.R
         → final-output-data/all-data-cleaned-geocoded.csv
         → final-output-data/filtered-years-cleaned-geocoded.csv
                  │
                  ▼
         analyze-and-export.R
         → final-output-data/filtered-years-relative-location-by-year.csv
         → final-output-data/filtered-years-relative-location-by-publication-total.csv
         → final-output-data/gg-ranks.csv
         → final-output-data/bd-ranks.csv
```

---

## Configuration (`run-pipeline.R`)

| Variable | Default | What it does |
|---|---|---|
| `YEARS` | `c(1975, 1977, ..., 1991)` | Which publication years to include |
| `DAMRON_DB_PATH` | `"mgg.db"` | Path to the Bob Damron SQLite database |
| `SKIP_GEOCODING` | `TRUE` | If TRUE, use the cache only. Set FALSE when you have new locations. |
| `CLEANING_FOLDER` | `"full-data-processing"` | Where corrections and lookup files live |
| `GEOCODING_FOLDER` | `"full-data-processing/geocoding-files"` | Where the geocoding cache lives |
| `OUTPUT_FOLDER` | `"full-data-processing/final-output-data"` | Where output CSVs are written |
| `INTERMEDIATE_FOLDER` | `"full-data-processing/intermediate"` | Where `raw-combined.csv` is written between layers |

---

## Layer 2: Validated Ingest (`validate-and-export.R`)

**Input:** GG CSVs + `mgg.db`
**Output:** `intermediate/raw-combined.csv`

This layer loads both data sources and combines them into a single dataframe with a consistent schema.

**Gaia's Guide** is loaded from CSV files in `GG-Data/` (one per year, named `gg-YYYY.csv`). Each CSV may have slightly different columns from year to year — missing columns are added as `NA`. Record IDs are generated here in the format `g-YEAR-NNNNN`.

**Bob Damron** is loaded directly from `mgg.db` via SQL. Types are stored in a separate table (`location_types`) linked via `location_type_assignments`, so the query joins them and collapses multiple types per location into a comma-separated string. Damron records already have record IDs in the DB (`unique_id`, format `d-YEAR-NNNNN`), so no ID generation is needed for that source.

After loading, the layer runs **validation**:
- Warns about rows missing `state` or with non-numeric `year`
- Stops the pipeline if duplicate `record_id` values are found

Finally, lat/lon swaps are corrected (vectorized) and `raw-combined.csv` is written.

---

## Layer 3: Cleaning & Enrichment (`clean-and-geocode.R`)

**Input:** `intermediate/raw-combined.csv`
**Output:** `final-output-data/all-data-cleaned-geocoded.csv`, `filtered-years-cleaned-geocoded.csv`

This is the most complex layer. It runs in sequence:

### 1. City Corrections

Reads `full-data-processing/unique-cities-replacements.csv` (3,772 entries). Each row is `city, new.city, state`. Only rows where `new.city ≠ city` are applied — the rest are no-ops kept for reference. A row-count assertion runs after the join to catch any accidental row duplication.

To add a correction: find the row for that city+state and update the `new.city` value.

### 2. Geocoding

Geocoding results are cached in `full-data-processing/geocoding-files/unique-locations-geocoded.csv`. On each run, the pipeline checks which city+state combinations in the data are missing from the cache, and (if `SKIP_GEOCODING = FALSE`) calls the Google Maps API for those only. Results are appended to the cache so each location is only geocoded once.

The Google API key is read from a `.env` file in the repo root:
```
GOOGLE_MAPS_API_KEY=your_key_here
```

After geocoding, the cache is merged back into the main dataframe on `geocode.value` (the `"City, ST"` string used as the lookup key).

### 3. Canonical City (if cache is enriched)

If `enrich-geocode-cache.R` has been run (see below), the geocoding cache will have `locality` and `is_neighborhood` columns. The pipeline uses these to add three new columns:

| Column | Description | Example |
|---|---|---|
| `canonical_city` | Google-derived municipality (parent city for neighbourhoods, same as input for independent cities) | `"Los Angeles"` for a Hollywood record |
| `is_neighborhood` | `TRUE` if the input was a neighbourhood | `TRUE` |
| `metro_city` | Optional override for metro-level grouping (PI-controlled) | `"Los Angeles"` for West Hollywood, if added to overrides |

### 4. Type Normalization and Category Mapping

The `type` column is messy — values vary across years and between GG and Damron. This step:
1. Lowercases and trims whitespace
2. Replaces non-ASCII characters and symbols
3. Joins against `full-data-processing/type-category/type-category-lookup.csv` to map raw type strings to one of 15 standardized categories (e.g. `"bar"`, `"restaurant"`, `"cruising areas"`)

The cleaned type replaces the original `type` column. A count of unmatched types (types present in the data but not in the lookup) is printed for review.

---

## Layer 4: Analysis & Export (`analyze-and-export.R`)

**Input:** `final-output-data/all-data-cleaned-geocoded.csv` (read once, held in memory)
**Output:** ranked and relative-frequency CSVs

This layer calculates two sets of relative data — both filtered to `YEARS`:

- **By year:** For each publication and year, what percentage of that year's total listings does each location account for? Locations are also ranked within each year.
- **Across all years:** Same calculation but aggregated across all years combined, per publication.

Per-publication ranked lists (`gg-ranks.csv`, `bd-ranks.csv`) are also exported for convenience.

---

## Key Supporting Files

| File | Purpose |
|---|---|
| `full-data-processing/unique-cities-replacements.csv` | City name corrections lookup (3,772 entries). Edit `new.city` to apply a correction. |
| `full-data-processing/geocoding-files/unique-locations-geocoded.csv` | Geocoding cache (~3,700 entries). Do not edit manually. |
| `full-data-processing/type-category/type-category-lookup.csv` | Maps raw type strings to 15 categories (~2,000 entries). |
| `full-data-processing/metro-overrides.csv` | Optional PI-controlled table to consolidate independent cities into metro areas (e.g. West Hollywood → Los Angeles). Starts empty. |
| `mgg.db` | SQLite database of Bob Damron records. Read-only in this pipeline. |
| `GG-Data/gg-YYYY.csv` | Gaia's Guide data, one CSV per year. |

---

## The Geocoding Cache Enrichment (one-time setup)

Before canonical city derivation works, you need to run `enrich-geocode-cache.R` once:

```bash
Rscript full-data-processing/enrich-geocode-cache.R
```

This script reads the `geoAddress` field that Google returns and classifies each location:

- A **3-part** address (`west hollywood, ca, usa`) → independent city → `locality = "West Hollywood"`, `is_neighborhood = FALSE`
- A **4-part** address (`hollywood, los angeles, ca, usa`) → neighbourhood → `locality = "Los Angeles"`, `is_neighborhood = TRUE`

It adds `locality` and `is_neighborhood` columns to the cache CSV and prints a diagnostic report showing which locations will be remapped. Re-run this script whenever new locations have been geocoded.

---

## Google Sheets Template for New GG Transcription

For new Gaia's Guide years, use the canonical column schema defined in `GG-Data/gg-template.csv` instead of exporting from Airtable. This avoids the column name drift that affected earlier years.

### Canonical columns

| Column | Type | Notes |
|---|---|---|
| `title` | text | Name of the listing |
| `type` | text | Use controlled vocabulary where possible (see type-category-lookup.csv) |
| `address` | text | Street address |
| `city` | text | City name |
| `state` | text | 2-letter state code (use dropdown validation in Sheets) |
| `description` | text | Descriptive text from the guide |
| `stars` | text | Star rating symbol(s) as printed |
| `star.type` | text | Type of star rating (e.g. D, \*, PT) |
| `notes` | text | Transcriber notes |
| `year` | number | Publication year — pre-fill this column for each sheet |
| `unclear.address` | TRUE/FALSE | Flag if address is hard to read |
| `statewide.address` | TRUE/FALSE | Flag if listing is statewide rather than a specific city |
| `mentions.race` | TRUE/FALSE | Whether the listing mentions race |
| `mentions.disability` | TRUE/FALSE | Whether the listing mentions disability |
| `transcription.method` | text | `"manual"` or `"ai-assisted"` |

### Setting up the Google Sheet

1. Open `GG-Data/gg-template.csv` and import it into a new Google Sheet
2. Add data validation:
   - **`state`**: dropdown restricted to the 50 state codes + DC + territories
   - **`unclear.address`, `statewide.address`, `mentions.race`, `mentions.disability`**: checkbox (renders as TRUE/FALSE on export)
   - **`transcription.method`**: dropdown with options `manual`, `ai-assisted`
3. Pre-fill the `year` column for the entire sheet
4. Lock the header row and protect column headers from editing
5. When the year is complete, export as CSV and save as `GG-Data/gg-YYYY.csv`

### Backward compatibility with old CSVs

The pipeline handles all pre-template column name variants automatically via `normalize_gg_columns()` in `functions.R`. You do not need to manually rename columns in existing CSVs. The mapping it handles includes:

| Old name (Airtable) | Canonical name |
|---|---|
| `Mentions Race?` | `mentions.race` |
| `Mentions Disability?` | `mentions.disability` |
| `mention.disability` (1989 typo) | `mentions.disability` |
| `Star Type` | `star.type` |
| `Last Modified`, `Last.Modified` | dropped |
| `Last Modified By`, `Last.Modified.By` | dropped |
| `Notes.2` | dropped |
| `Questions/Unsure Dr. R should check` | dropped |
| `Start date` | dropped |
| `Feminist Bookstores` | dropped |

---

## Adding a New Year of Data

1. Add the new year to `YEARS` in `run-pipeline.R`
2. Place the GG CSV at `GG-Data/gg-YYYY.csv` (Damron data comes from `mgg.db` automatically)
3. Run the pipeline with `SKIP_GEOCODING = TRUE` first to see how many new locations need geocoding
4. Review `full-data-processing/unique-cities-cleaning.csv` — any new city+state pairs not in the corrections lookup will appear here
5. Add corrections to `unique-cities-replacements.csv` if needed, then re-run
6. Set `SKIP_GEOCODING = FALSE` and re-run to geocode new locations (requires API key in `.env`)
7. Re-run `enrich-geocode-cache.R` to classify any newly geocoded locations

---

## Required R Packages

```r
install.packages(c("tidyverse", "ggmap", "DBI", "RSQLite", "textclean"))
```

`ggmap` is used for Google geocoding. `DBI` + `RSQLite` read from `mgg.db`. `textclean` normalizes type strings.

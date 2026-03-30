# MGG-LC-Data Pipeline Review & Improvement Plan

*Generated 2026-03-26 via code review with Claude*

---

## Executive Summary

We reviewed the entire MGG-LC-Data repository — the R processing pipeline, data files, folder structure, and its relationship to the MGG-Site Flask app. The current `data-processing.R` script has several bugs that prevent it from running correctly, the Gaia's Guide CSVs have inconsistent schemas due to Airtable export drift, and there's no formal process for team members to flag or correct data errors.

This document outlines what we found, a proposed redesigned pipeline, and a phased implementation plan.

---

## Part 1: Current State Assessment

### What works well
- The overall data flow (load → clean → geocode → categorize → analyze) is sound
- Geocoding cache (`unique-locations-geocoded.csv`) prevents redundant API calls
- City corrections via lookup table is a good pattern
- Type-to-category mapping with 15 consolidated categories is well-designed
- Record ID scheme (`g-YEAR-NNNNN`, `d-YEAR-NNNNN`) is clear and useful

### Bugs found in `data-processing.R`

1. **type_category_lookup used before it's loaded** (line 498 vs 518): The type-to-category join happens before the lookup table is read in. The join is also duplicated later. This means categories aren't being mapped correctly.

2. **`generate_unique_cities_list()` is called but never defined** (line 442): The pipeline will error out at this step. The function was likely lost during a refactor.

3. **Corrections file points to test data** (lines 449, 453): References `unique-cities-replacements-testing.csv` (11 rows) instead of the production file with 3,772 corrections.

### Schema drift in Gaia's Guide CSVs

Each year's Airtable export has different columns:
- 1975: `statewide.address`, `Mentions Race?`, `Start date`
- 1983: `Last.Modified`, `Notes.2`
- 1985: `Star Type` (space, not dot), `Questions/Unsure Dr. R should check`
- 1989: `mentions.race` (lowercase, different punctuation)
- Column naming is inconsistent: `Mentions Race?` vs `mentions.race` vs `Mentions.Race.`

The `load_gg_data()` function handles missing columns by adding NAs, but doesn't normalize names — so variant columns get silently dropped.

### Other issues
- No dependency management (no `renv.lock`) — package versions aren't pinned
- Google API key entered interactively via `readline()` — blocks automated runs
- `apply_city_corrections()` uses `left_join` which can silently duplicate rows if the lookup has duplicate entries
- `fix_lat_lon_swaps()` loops row-by-row instead of vectorizing (slow on 200K rows)
- Relative data functions re-read CSVs from disk instead of using in-memory data
- `.gitignore` doesn't exclude `.env` files (API keys could be committed)
- Bob Damron data still loaded from an old RDS file, while the source of truth is now the SQLite DB in MGG-Site

---

## Part 2: Proposed Redesigned Pipeline

### Architecture overview

```
┌─────────────────────────────────────────────────────────┐
│  LAYER 1: TRANSCRIPTION & DATA ENTRY                    │
│                                                         │
│  Gaia's Guide:  Google Sheets (locked schema)           │
│                 + optional AI-assisted draft             │
│  Bob Damron:    SQLite DB in MGG-Site (complete)        │
└──────────────────────┬──────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────┐
│  LAYER 2: VALIDATED INGEST                              │
│                                                         │
│  validate-and-export.R                                  │
│  - Pulls GG data from CSVs, validates schema            │
│  - Pulls Damron data from SQLite DB directly            │
│  - Enforces canonical column names                      │
│  - Rejects bad data with clear error messages           │
│  - Outputs: raw-combined.csv (validated, not cleaned)   │
└──────────────────────┬──────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────┐
│  LAYER 3: CLEANING & ENRICHMENT                         │
│                                                         │
│  clean-and-geocode.R                                    │
│  - City corrections (single canonical lookup file)      │
│  - Geocoding (cached, non-interactive API key)          │
│  - Type normalization + category mapping                │
│  - Census region/division joins                         │
│  - Outputs: cleaned-geocoded.csv                        │
└──────────────────────┬──────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────┐
│  LAYER 4: ANALYSIS & EXPORT                             │
│                                                         │
│  analyze-and-export.R                                   │
│  - Relative frequency calculations                      │
│  - Rankings by year and across years                    │
│  - Filtered subsets (matching years only, etc.)          │
│  - Data quality report (summary stats, missing values)  │
│  - Outputs: final CSVs + quality-report.html            │
└──────────────────────┬──────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────┐
│  LAYER 5: WEBSITE SYNC                                  │
│                                                         │
│  sync-to-db.py (in MGG-Site repo)                       │
│  - Reads cleaned-geocoded.csv                           │
│  - Diffs against current DB state                       │
│  - Updates SQLite with new/changed records              │
│  - Supports corrections flowing back via a flag file    │
└─────────────────────────────────────────────────────────┘
```

### Layer 1: Transcription & Data Entry

**Gaia's Guide — Move from Airtable to Google Sheets**

Why Google Sheets:
- Free, widely accessible
- Data validation rules enforce schema at entry time (dropdowns for state codes, restricted type values)
- No schema drift — columns are locked, team members fill in rows
- Consistent CSV exports with stable column names
- Comment/annotation features for flagging uncertain entries

Template columns:
- `title` (text), `type` (dropdown), `address` (text), `city` (text), `state` (dropdown: 2-letter codes), `description` (text), `stars` (dropdown), `star.type` (text), `notes` (text), `year` (pre-filled)
- `unclear.address`, `statewide.address` as checkboxes
- Optional: `mentions.race`, `mentions.disability` as checkboxes
- Drop Airtable artifacts: `Last.Modified`, `Last.Modified.By`, `Notes.2`, `Questions/Unsure`, `Start date`

**AI-assisted transcription for remaining issues:**
- Scan pages → OCR (Google Cloud Vision or GPT-4o) → LLM extracts structured fields
- Human reviewer verifies every entry against original
- Add `transcription.method` column ("manual" or "ai-assisted") for scholarly transparency

**Bob Damron — Read directly from SQLite**
- Replace the stale RDS file with a direct connection to `../MGG-Site/mgg.db`
- Eliminates the stale-data problem entirely

### Layer 2: Validated Ingest

A single script that pulls from both sources, validates, and combines:
- Connects to SQLite via `RSQLite`/`DBI` packages
- Reads GG CSVs and normalizes column names automatically
- Fails loudly on schema violations
- Validation: every record has city+state, year is numeric, state is valid, no duplicate IDs

### Layer 3: Cleaning & Enrichment

Key improvements:
- **One corrections file** instead of three (currently `unique-cities-replacements.csv`, `unique-cities-replacements-testing.csv`, and root-level copy all exist with different schemas)
- **Non-interactive geocoding** — API key from `.env` file, not `readline()`
- **Row-count assertions** after every join to catch duplication bugs
- **Vectorized operations** instead of row-by-row loops
- **Type categorization done once**, in the correct order

### Layer 4: Analysis & Export

- Operates on in-memory data (no re-reading CSVs from disk)
- Generates a **data quality report** (HTML via Quarto) that non-technical team members can review:
  - Record counts by publication and year
  - Geocoding coverage
  - Unmatched types
  - Corrections applied
  - Missing data summary

### Layer 5: Website Sync

A diff-based sync script that compares output against current DB state, reports what would change, and applies updates. Lives in MGG-Site repo.

### Single entry point: `run-pipeline.R`

```r
# Configuration
YEARS <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989, 1991)
DAMRON_DB_PATH <- "../MGG-Site/mgg.db"
SKIP_GEOCODING <- TRUE  # Set FALSE when adding new locations

source("full-data-processing/functions.R")
source("full-data-processing/validate-and-export.R")
source("full-data-processing/clean-and-geocode.R")
source("full-data-processing/analyze-and-export.R")
```

### What changes for each team role

| Role | Current workflow | New workflow |
|------|-----------------|--------------|
| **Transcriber** | Enter data in Airtable, export CSV, hand off | Enter data in Google Sheet template with validation. Download CSV when done. |
| **Pipeline manager** | Run 647-line R script interactively, debug errors | Run `run-pipeline.R`, review `quality-report.html` |
| **Researcher/analyst** | Use output CSVs | Same, with a quality report confirming data integrity |
| **Web developer** | Manually import CSVs into SQLite | Run sync script that shows a diff before applying |

---

## Part 3: Corrections Workflow (Open Question)

**The problem:** When a research assistant spots an error (e.g., "Birminghm" should be "Birmingham"), there's currently no way for them to flag or fix it without technical database access.

**Options discussed (team needs to decide):**

### Option A: Shared corrections spreadsheet
A Google Sheet where anyone logs errors: record_id, field, current value, correct value, notes, status. Pipeline manager reviews and applies periodically.
- **Pro:** Lowest technical barrier, familiar interface
- **Con:** Manual application, no automation

### Option B: Corrections CSV in git repo
A `corrections.csv` in the repo that RAs edit (via GitHub web UI). Pipeline reads and applies automatically.
- **Pro:** Version-controlled, automated
- **Con:** Requires minimal git/GitHub comfort

### Option C: Read-only data sheets + corrections log
Export Damron data to Google Sheets for review (read-only). Team logs corrections in a separate tab. Script applies corrections to DB.
- **Pro:** Full data visible for review, DB stays source of truth
- **Con:** Large spreadsheets (some years have 10K+ rows, Google Sheets slows above 5K)

### Option D: Editable sheets + reimport
Export each year of Damron to Google Sheets. Team edits directly. Reimport corrected data.
- **Pro:** Most familiar editing experience
- **Con:** Sync complexity (two sources of truth), risk of accidental data loss, large spreadsheets

### Option E: Admin interface on MGG-Site
Build an edit UI into the Flask website for authorized team members.
- **Pro:** Most powerful, direct editing
- **Con:** Most development work

**Also to decide:** Should corrections fix the source data permanently, or be applied as a separate transformation layer during processing? The scholarly argument for a transformation layer is that it preserves the original transcription; the practical argument for fixing the source is simplicity.

---

## Part 4: Implementation Phases

### Phase 1: Fix bugs in current pipeline
*Can be done now — makes the existing script runnable*

1. Fix `type_category_lookup` ordering (move load before first use, remove duplicate join)
2. Define missing `generate_unique_cities_list()` function
3. Switch from test corrections file to production file
4. Add `.env` to `.gitignore`

### Phase 2: Restructure pipeline + connect to SQLite
5. Split `data-processing.R` into `functions.R` + layered scripts
6. Replace RDS file with direct SQLite connection
7. Consolidate city corrections into one canonical file
8. Parameterize configuration (API key, years, paths)
9. Add validation checks and row-count assertions

### Phase 3: Transcription improvements
10. Create Google Sheets template with data validation
11. Test AI-assisted transcription on one issue
12. Add column name standardization for backward compat with existing CSVs

### Phase 4: Quality, documentation, and corrections
13. Create data quality report (Quarto)
14. Design and implement chosen corrections workflow
15. Update README
16. Add `renv` for R dependency management

### Phase 5: Website integration
17. Create diff-based sync script in MGG-Site
18. Test end-to-end flow

---

## Appendix: Key Files Reference

| File | Location | Purpose |
|------|----------|---------|
| `data-processing.R` | `full-data-processing/` | Current (buggy) pipeline — to be restructured |
| `mgg-data.rds` | `MGG-Data/` | Old Damron data snapshot — to be replaced with SQLite connection |
| `mgg.db` | `../MGG-Site/` | Current Damron source of truth (SQLite) |
| `import_data.py` | `../MGG-Site/data/scripts/` | Current CSV-to-DB import script |
| `gg-*.csv` | `GG-Data/` | Gaia's Guide data files (9 years, varying schemas) |
| `unique-cities-replacements.csv` | `full-data-processing/` | Production city corrections (3,772 entries) |
| `unique-cities-replacements-testing.csv` | `full-data-processing/` | Test corrections file (11 entries) — currently referenced by mistake |
| `type-category-lookup.csv` | `full-data-processing/type-category/` | Type-to-category mapping (~2,000 entries, 15 categories) |
| `unique-locations-geocoded.csv` | `full-data-processing/geocoding-files/` | Geocoding cache (3,000+ locations) |
| `state_census_region_division.csv` | `full-data-processing/geocoding-files/` | US Census regions lookup |

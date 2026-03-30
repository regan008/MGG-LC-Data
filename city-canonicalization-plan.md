# Plan: Derive Canonical City Names from Geocoding Data

*Generated 2026-03-30 via code review with Claude*

---

## The Problem

In the Damron guidebook, locations were listed under a parent city (e.g., "Los Angeles") with the neighborhood in parentheses (e.g., "(Studio City)"). During transcription, sometimes the parent city was recorded and sometimes the neighborhood. This creates inconsistency: "Hollywood, CA" and "Los Angeles, CA" appear as different locations in the dataset, fragmenting counts and making cross-guide comparison unreliable.

This affects ~9,000 records across LA, SF, NYC, Chicago, and Miami metros (~4.5% of the dataset). Currently, manual `fct_collapse()` logic in `mgg-clean.R` handles only 4 states with hardcoded neighborhood lists. We need a comprehensive, automated solution.

---

## Key Discovery

**The geocoding cache already contains the answer.** The `geoAddress` field from Google's geocoder embeds the parent city in its response format:

| geocode.value (our input) | geoAddress (Google's response) | Parts | What this tells us |
|---|---|---|---|
| `Hollywood, CA` | `hollywood, los angeles, ca, usa` | 4 | Neighborhood of Los Angeles |
| `West Hollywood, CA` | `west hollywood, ca, usa` | 3 | Independent city (not a neighborhood) |
| `Studio City, CA` | `studio city, los angeles, ca, usa` | 4 | Neighborhood of Los Angeles |
| `Greenwich Village, NY` | `greenwich village, new york, ny, usa` | 4 | Neighborhood of New York |
| `Berkeley, CA` | `berkeley, ca, usa` | 3 | Independent city |
| `North Hollywood, CA` | `north hollywood, los angeles, ca, usa` | 4 | Neighborhood of Los Angeles |
| `The Castro, CA` | `the castro, san francisco, ca 94114, usa` | 4 | Neighborhood of San Francisco |

**The rule is simple:** When Google returns a 4-part address (`neighborhood, city, state, country`), the 2nd part is the parent city. When it returns a 3-part address (`city, state, country`), the location IS an independent city. Google automatically distinguishes real neighborhoods from independent municipalities.

**No new API calls needed** for the ~3,700 locations already in our geocoding cache — we just need to parse what Google already told us.

---

## Proposed Approach: Three-Tier City System

We preserve the original transcription while adding canonical names at two additional levels:

1. **`city`** — Original transcribed value (unchanged, for scholarly provenance)
2. **`canonical_city`** — Derived from Google's geocoder (the real municipal city: "Los Angeles" for Hollywood, but "West Hollywood" stays as-is because it's genuinely a separate city)
3. **`metro_city`** — Optional override for metro-level analysis (consolidates independent suburban cities like West Hollywood, Santa Monica into "Los Angeles" when that level of aggregation is desired)

Researchers choose which level to use depending on their analysis. For cross-guide comparison, `canonical_city` is the default. For metro-level spatial analysis, `metro_city` provides maximum consolidation.

---

## Implementation Steps

### Step 1: Enrich the geocoding cache (one-time script)

Create a script (`enrich-geocode-cache.R`) that reads the existing geocoding cache (~3,700 entries) and adds two new columns by parsing the `geoAddress` field:

- **`locality`** — the canonical city name (e.g., "Los Angeles" for Hollywood entries)
- **`is_neighborhood`** — boolean flag (TRUE if Google returned a 4-part address)

The parsing handles edge cases like zip codes in addresses (e.g., `"the castro, san francisco, ca 94114, usa"`) and title-cases the results.

For the ~137 entries where parsing is ambiguous (2-part addresses, state-level entries, etc.), we can optionally use reverse geocoding to get the structured city name from the API.

The script produces a diagnostic report showing how many neighborhoods were found and which locations differ from their canonical city.

### Step 2: Create a metro overrides table

A small, reviewable CSV file (`metro-overrides.csv`) for cases where the project wants to consolidate independent cities into their metro area for comparison:

```
locality,state,metro_city
West Hollywood,CA,Los Angeles
Santa Monica,CA,Los Angeles
Beverly Hills,CA,Los Angeles
Culver City,CA,Los Angeles
Burbank,CA,Los Angeles
```

**This is a scholarly decision** — the PI should review and decide which independent cities to consolidate. The table can start empty and be populated over time as analysis needs dictate. Each entry is explicit and auditable.

### Step 3: Update the processing pipeline

Modify `data-processing.R` to:
- Carry through `locality` and `is_neighborhood` from the enriched cache during the geocoding merge
- Derive `canonical_city` (use locality if available, fall back to original city)
- Apply metro overrides to produce `metro_city`
- Create `canonical_geocode_value` (e.g., "Los Angeles, CA") for grouping in relative frequency calculations
- Update future geocoding calls to use `output = "more"` so new locations get `locality` directly from the API

### Step 4: Migrate typo corrections from mgg-clean.R

The existing `fct_collapse()` logic in `mgg-clean.R` does two things:
- **Typo corrections** (e.g., "Alleghanny" → "Alleghany", "Binghampton" → "Binghamton") — these should move into the existing city corrections lookup table
- **Neighborhood consolidation** (e.g., "Hollywood" → "Los Angeles") — this is now handled automatically by the canonical_city derivation

After migration, the manual neighborhood consolidation logic can be retired.

### Step 5: Validate

1. Run the enrichment script and review its diagnostic report
2. Cross-check: every neighborhood in `mgg-clean.R`'s hardcoded lists should now be automatically detected
3. Spot-check that independent cities (West Hollywood, Santa Monica, Berkeley, Cambridge) are NOT incorrectly marked as neighborhoods
4. Run the full pipeline and compare relative data outputs before/after — LA, SF, NYC totals should increase as neighborhoods are consolidated
5. Verify the original `city` column is completely unchanged

---

## Edge Cases

| Case | Example | How it's handled |
|---|---|---|
| Independent city near a metro | West Hollywood, CA | `canonical_city = "West Hollywood"` (correct — it's a real city). Override to "Los Angeles" via metro-overrides.csv only if desired for analysis. |
| Zip code in Google's response | `the castro, san francisco, ca 94114, usa` | Strip zip codes before counting address parts |
| Misspelled input city | `"San Francsico, CA"` → Google returns `"san francisco, ca, usa"` | Works perfectly — locality comes from Google's corrected output |
| Non-US locations | Puerto Rico, Guam | Different address formats — fall back to original city |
| Failed geocode | `geoAddress = NA` | `locality = NA`, use original city |
| State-level entry | `"AL, United States"` → `"alabama, usa"` (2 parts) | `locality = NA`, use original |

---

## New Columns in Output Data

After implementation, the final output CSVs will include:

| Column | Description | Example for a Hollywood bar |
|---|---|---|
| `city` | Original transcription (unchanged) | Hollywood |
| `canonical_city` | Google-derived municipality | Los Angeles |
| `is_neighborhood` | Whether the original was a neighborhood | TRUE |
| `canonical_geocode_value` | For grouping/comparison | Los Angeles, CA |
| `metro_city` | Metro-level consolidation (if override exists) | Los Angeles |

---

## How This Replaces Current Manual Approach

**Current (`mgg-clean.R`):**
- Hardcoded lists for 4 states only (CA, NY, FL, IL)
- Mixes typo corrections with neighborhood consolidation
- Doesn't distinguish real neighborhoods from independent cities
- Must be manually updated for each new neighborhood discovered
- Only applies to Damron data, not Gaia's Guide

**Proposed:**
- Works automatically for ALL states and ALL locations
- Uses Google's authoritative municipal boundaries
- Correctly identifies independent cities (West Hollywood, Berkeley) vs. neighborhoods (Hollywood, Studio City)
- No manual updates needed — new locations get classified at geocoding time
- Applies to both Damron and Gaia's Guide data equally

---

## Dependencies & Sequencing

This work can be started immediately — the enrichment script (Step 1) only reads and writes the geocoding cache CSV and has no dependencies on other pipeline changes.

Pipeline integration (Step 3) should happen after the existing bugs in `data-processing.R` are fixed (documented in `pipeline-review-and-plan.md`).

**Key files involved:**
- `full-data-processing/geocoding-files/unique-locations-geocoded.csv` — geocoding cache to enrich
- `full-data-processing/data-processing.R` — pipeline to update
- `MGG-Data/mgg-clean.R` — manual consolidation logic to retire
- `full-data-processing/unique-cities-replacements.csv` — absorbs typo corrections from mgg-clean.R

---

## Open Questions for Team Discussion

1. **Metro overrides scope:** Which independent cities should be consolidated at the `metro_city` level? This is a research decision. Should West Hollywood be grouped with Los Angeles? What about Long Beach? Santa Monica?

2. **Cross-state metros:** Should Hoboken/Jersey City (NJ) be consolidated into New York City (NY) at the metro level? This crosses state boundaries.

3. **Historical vs. modern boundaries:** Google returns modern municipal boundaries. A neighborhood that was independent in 1975 but was later incorporated might be classified differently than its historical status. Is modern classification acceptable for the project's purposes?

4. **Gaia's Guide consistency:** Does Gaia's Guide have the same neighborhood-vs-city problem, or is it primarily a Damron transcription issue? The solution applies to both, but it's worth knowing the scope.

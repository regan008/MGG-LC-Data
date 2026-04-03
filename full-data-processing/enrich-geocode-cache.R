# PHASE 3a: ENRICH GEOCODING CACHE
#
# Parses the geoAddress field in unique-locations-geocoded.csv to derive two
# new columns:
#
#   locality       — canonical city name from Google (e.g. "Los Angeles" for
#                    a Hollywood input, "West Hollywood" for West Hollywood)
#   is_neighborhood — TRUE when Google returned a 4-part address, meaning the
#                    input was a neighbourhood of a larger city
#
# Rule: Google's geoAddress format distinguishes neighbourhoods from cities:
#   4-part: "hollywood, los angeles, ca, usa"  → neighbourhood of Los Angeles
#   3-part: "west hollywood, ca, usa"          → independent city
#
# Run this script ONCE (or after adding new geocoded locations).
# It is safe to re-run — it only adds/updates the two new columns.

library(tidyverse)

CACHE_FILE <- "full-data-processing/geocoding-files/unique-locations-geocoded.csv"

cat("Reading geocoding cache...\n")
cache <- read.csv(CACHE_FILE, stringsAsFactors = FALSE)
cat(sprintf("  %d entries loaded.\n", nrow(cache)))

# ---------------------------------------------------------------------------
# Parse geoAddress → locality + is_neighborhood
# ---------------------------------------------------------------------------

parse_geo_address <- function(geo_address) {
  # Returns a list with: locality (character), is_neighborhood (logical)

  if (is.na(geo_address) || geo_address == "" || geo_address == "NA") {
    return(list(locality = NA_character_, is_neighborhood = NA))
  }

  # Strip zip codes embedded in the address (e.g. "ca 94114" or "ma 01002")
  cleaned <- gsub("\\b[a-z]{2}\\s+\\d{5}\\b", "", geo_address)
  # Normalise multiple spaces / trailing commas left by the strip
  cleaned <- gsub(",\\s*,", ",", cleaned)
  cleaned <- trimws(gsub("\\s+", " ", cleaned))

  parts <- strsplit(cleaned, ",\\s*")[[1]]
  parts <- parts[nzchar(trimws(parts))]  # drop empty elements

  if (length(parts) == 4) {
    # neighbourhood, city, state, country  →  independent city is parts[2]
    locality <- tools::toTitleCase(trimws(parts[2]))
    return(list(locality = locality, is_neighborhood = TRUE))
  } else if (length(parts) == 3) {
    # city, state, country  →  parts[1] IS the independent city
    locality <- tools::toTitleCase(trimws(parts[1]))
    return(list(locality = locality, is_neighborhood = FALSE))
  } else {
    # 2-part (state-level), 5+-part, or other edge case — can't determine
    return(list(locality = NA_character_, is_neighborhood = NA))
  }
}

cat("Parsing geoAddress fields...\n")
parsed <- lapply(cache$geoAddress, parse_geo_address)

cache$locality        <- sapply(parsed, `[[`, "locality")
cache$is_neighborhood <- sapply(parsed, `[[`, "is_neighborhood")

# ---------------------------------------------------------------------------
# Diagnostic report
# ---------------------------------------------------------------------------

n_total        <- nrow(cache)
n_neighborhood <- sum(cache$is_neighborhood == TRUE,  na.rm = TRUE)
n_independent  <- sum(cache$is_neighborhood == FALSE, na.rm = TRUE)
n_ambiguous    <- sum(is.na(cache$is_neighborhood))

cat("\n=== Enrichment Report ===\n")
cat(sprintf("  Total entries:         %d\n", n_total))
cat(sprintf("  Independent cities:    %d\n", n_independent))
cat(sprintf("  Neighbourhoods:        %d\n", n_neighborhood))
cat(sprintf("  Ambiguous/unparseable: %d\n", n_ambiguous))

# Locations where the canonical city differs from the geocode input
changed <- cache %>%
  filter(!is.na(locality) & is_neighborhood == TRUE) %>%
  mutate(input_city = sub(",.*", "", geocode.value)) %>%
  filter(tolower(trimws(input_city)) != tolower(locality)) %>%
  select(geocode.value, geoAddress, locality) %>%
  arrange(geocode.value)

cat(sprintf("\n  Neighbourhoods that will map to a different canonical city: %d\n",
            nrow(changed)))
if (nrow(changed) > 0) {
  print(changed, n = Inf)
}

# Spot-check: these should NOT be marked as neighbourhoods
independent_check <- c("West Hollywood, CA", "Santa Monica, CA", "Berkeley, CA",
                       "Cambridge, MA", "Hoboken, NJ")
cat("\n--- Independence spot-check ---\n")
for (loc in independent_check) {
  row <- cache[cache$geocode.value == loc, ]
  if (nrow(row) == 0) {
    cat(sprintf("  NOT IN CACHE: %s\n", loc))
  } else {
    cat(sprintf("  %-30s  is_neighborhood = %s  locality = %s\n",
                loc, row$is_neighborhood, row$locality))
  }
}

# ---------------------------------------------------------------------------
# Write enriched cache
# ---------------------------------------------------------------------------

write.csv(cache, CACHE_FILE, row.names = FALSE)
cat(sprintf("\nWrote enriched cache to %s\n", CACHE_FILE))

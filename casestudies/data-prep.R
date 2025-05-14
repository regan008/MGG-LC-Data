library(tidyverse)
library(ggmap)

#read in data for Gaia's Guide case studies
gg <- read.csv("casestudies/combined_GGdata_manual.csv")
gg <- gg %>%
  select(-X)

# flag for filtering out records where exact locations could not be found
gg_unclear_locations <- gg %>% filter(Status == "Location could not be verified. General city or location coordinates used.")

# read in data for Bob Damron's Address Book
mgg <- readRDS("casestudies/mgg-data.rds")
mgg <- mgg %>%
  select(-ID) %>%
  rename(year = Year) %>%
  rename(address = streetaddress) %>%
  rename(unclear.address = unclear_address)
mgg$publication <- "Bob Damron's Address Book"

# Manual corrections to the mgg data
bad_coords <- mgg %>% filter(address == "2121 San Jacinto #220", year == 1989)
mgg <- mgg %>%
  mutate(
    lat = ifelse(address == "2121 San Jacinto #220" & year == 1989, 32.7875205, lat),
    lon = ifelse(address == "2121 San Jacinto #220" & year == 1989, -96.7976, lon)
  )
bad_coords <- mgg %>% filter(address == "519 S.W. 3rd #505", year == 1987)
mgg <- mgg %>%
  mutate(
    lat = ifelse(address == "519 S.W. 3rd #505" & year == 1987, 45.519446247573555, lat),
    lon = ifelse(address == "519 S.W. 3rd #505" & year == 1987, -122.67519297895736, lon)
  )

# Get the columns in gg but not in mgg
columns_in_gg_not_in_mgg <- setdiff(names(gg), names(mgg))

# Get the columns in mgg but not in gg
columns_in_mgg_not_in_gg <- setdiff(names(mgg), names(gg))

# Print the results
cat("Columns in gg but not in mgg:\n")
print(columns_in_gg_not_in_mgg)

cat("Columns in mgg but not in gg:\n")
print(columns_in_mgg_not_in_gg)

# Filter the data by date and city
gg_filtered <- gg %>%
  filter(year > 1974 & year < 1990) %>%
  filter(city == "Detroit" | city == "Portland" | city == "Dallas") %>%
  filter(state == "MI" | state == "OR" | state == "TX")
mgg_filtered <- mgg %>%
  filter(year %in% unique(gg_filtered$year)) %>%
  filter(city == "Detroit" | city == "Portland" | city == "Dallas") %>%
  filter(state == "MI" | state == "OR" | state == "TX")

# Combine gg and mgg
gg_filtered <- gg_filtered %>% mutate(lat = as.numeric(lat))
mgg_filtered <- mgg_filtered %>% mutate(lat = as.numeric(lat))
combined_data <- bind_rows(gg_filtered, mgg_filtered)

# save data frame with GG and Damron data for case study cities and years
write.csv(combined_data, "casestudies/gg-mgg-case-studies.csv", row.names = FALSE)


# Filter the data for women's spaces
filtered_data <- combined_data %>%
  filter(
    publication == "Gaia's Guide" | # Keep all entries with Gaia's Guide
      (publication == "Bob Damron's Address Book" &
        (grepl("\\(G\\)", amenityfeatures) | # Filter for (G)
          grepl("\\(L-1980-1989\\)", amenityfeatures))) # Filter for the exact phrase (L - 1980-1989)
  )

# Save the filtered data to a new CSV file
#write.csv(filtered_data, "cs/gg-mgg-women-data.csv", row.names = FALSE)

library(tidyverse)
library(ggmap)

gg <- read.csv("cs/combined_GGdata.csv") %>% select(-X, -country, -unclear.address)
gg$amenityfeatures <- ""
mgg <- readRDS("cs/mgg-data.rds") %>%
  select(-ID, -status, -unclear_address) %>%
  rename(year = Year) %>%
  rename(address = streetaddress)
mgg$publication <- "Bob Damron's Address Book"

# Get the columns in gg but not in mgg
columns_in_gg_not_in_mgg <- setdiff(names(gg), names(mgg))

# Get the columns in mgg but not in gg
columns_in_mgg_not_in_gg <- setdiff(names(mgg), names(gg))

# Print the results
cat("Columns in gg but not in mgg:\n")
print(columns_in_gg_not_in_mgg)

cat("Columns in mgg but not in gg:\n")
print(columns_in_mgg_not_in_gg)

mgg <- mgg %>%
  filter(year == 1975:1989) %>%
  filter(city %in% c("Dallas", "Portland", "Detroit"))

# Combine gg and mgg
gg <- gg %>% mutate(lat = as.numeric(lat))
mgg <- mgg %>% mutate(lat = as.numeric(lat))
combined_data <- bind_rows(gg, mgg)

# save data frame with GG and all Damron data.
write.csv(combined_data, "cs/all-gg-mgg-data.csv")


# Filter the data
filtered_data <- combined_data %>%
  filter(
    publication == "Gaia's Guide" | # Keep all entries with Gaia's Guide
      (publication == "Bob Damron's Address Book" &
        (grepl("\\(G\\)", amenityfeatures) | # Filter for (G)
          grepl("\\(L-1980-1989\\)", amenityfeatures))) # Filter for the exact phrase (L - 1980-1989)
  )

# Save the filtered data to a new CSV file
write.csv(filtered_data, "cs/gg-mgg-women-data.csv", row.names = FALSE)

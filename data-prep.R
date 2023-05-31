library(tidyverse)
library(forcats)
library(ggmap)
lc.data <- read.csv(file = "spatial-data-lc-05-17-2023.csv")
mgg.data <- readRDS(file = "mgg-data-cleaned.rds")

mgg.data <- mgg.data %>% rename("year" = "Year", "mgg.type" = "type")
lc.data <- lc.data %>% rename("title" = "Primary.Entity", "notes" = "Contents.Notes..keywords..event..important.names.", "orig.lc.long" = "Longitude", "orig.lc.lat" = "Latitude", "city" = "City", "state" = "State", "country" = "Country", "year" = "Year","publication" = "Publication")

# Get column names
mgg.data.names <- names(mgg.data)
lc.data.names <- names(lc.data)

# Column names that are the same in both dataframes
common_cols <- intersect(mgg.data.names, lc.data.names)
print("Column names that are the same in both dataframes:")
print(common_cols)

# Column names that are only in df1
mgg_only_cols <- setdiff(mgg.data.names, lc.data.names)
print("Column names that are only in df1:")
print(mgg_only_cols)

# Column names that are only in df2
lc_only_cols <- setdiff(lc.data.names, mgg.data.names)
print("Column names that are only in df2:")
print(lc_only_cols)

# Combine dataframes and replace missing values with NA
combined_data <- merge(lc.data, mgg.data, by = common_cols, all = TRUE)

# Replacing missing values with NA
combined_data[is.na(combined_data)] <- NA

# Print the combined dataframe
print(combined_data)

combined_data$geocode.value <-  paste(combined_data$city, ", ", combined_data$state, ", ", combined_data$country, sep="")

unique_cities <- unique(combined_data$geocode.value) %>% as.data.frame()
unique_cities <- unique_cities %>% rename("geocode.value" = ".")



register_google(key = Sys.getenv("MGG_GOOGLE_KEY"))
#geocoding function
for(i in 1:nrow(unique_cities)) {
  # Print("Working...")
  result <- tryCatch(geocode(unique_cities$geocode.value[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  unique_cities$lon[i] <- as.numeric(result[1])
  unique_cities$lat[i] <- as.numeric(result[2])
  unique_cities$geoAddress[i] <- as.character(result[3])
}

write.csv(unique_cities, file="unique_city_list.csv", row.names = FALSE)
geocoded_df <- left_join(combined_data, unique_cities, by = "geocode.value")
write.csv(geocoded_df, file ="geocoded_data.csv", row.names = FALSE)
geocoded.data <- read.csv("geocoded_data.csv")

geocoded.data <- geocoded.data %>% mutate(publication = fct_collapse(geocoded.data$publication, 
                                            "Lesbian Connection" = c(" Lesbian Connection", "Lesbian Connections")))
                                            
total.per.year <- geocoded.data %>% group_by(publication, year) %>% summarize(pub.count = n())
total.per.loc.byyear <- geocoded.data %>% group_by(publication, year, city, state, country) %>% summarize(count = n())

relative.count <- full_join(total.per.year, total.per.loc.byyear)
relative.count <- relative.count %>% mutate(relative.percentage = count/pub.count * 100)
relative.count$geocode.value <-  paste(relative.count$city, ", ", relative.count$state, ", ", relative.count$country, sep="")
relative.count <- left_join(relative.count, unique_cities, by = "geocode.value")
write.csv(relative.count, file="relative-data.csv", row.names = FALSE)

# of location in a city / # of location in a year 


geocoded.w.data <- geocoded.data %>% filter(grepl("(G)", geocoded.data$amenityfeatures, fixed = TRUE) | grepl("(L)", geocoded.data$amenityfeatures, fixed = TRUE)
                                            
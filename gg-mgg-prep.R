library(tidyverse)
library(forcats)
library(ggmap)

#load data and add a publication and country column to match the other datasets
gg.data <- read.csv("gg-1983.csv", header = TRUE)
gg.data$publication <- "Gaia's Guide"
gg.data$country <- "United States"

#create a geocode value from the city state and country
gg.data$geocode.value <-  paste(gg.data$city, ", ", gg.data$state, ", ", gg.data$country, sep="")

# read in a list of unique cities (geocode values) from Damron and LC data. Join it with Gaia's guide data.
unique.cities <- read.csv("unique_city_list.csv")
gg.geocode <- left_join(gg.data, unique.cities, by="geocode.value")

#find all the empty rows that didn't have a match. 
gg.geocode.empty <- gg.geocode %>% filter(is.na(lon))
new.geocode.entries <- unique(gg.geocode.empty$geocode.value) %>% as.data.frame() %>% rename("geocode.value" = ".")

register_google(key = Sys.getenv("MGG_GOOGLE_KEY"))
#geocoding function
for(i in 1:nrow(new.geocode.entries)) {
  # Print("Working...")
  result <- tryCatch(geocode(new.geocode.entries$geocode.value[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  new.geocode.entries$lon[i] <- as.numeric(result[1])
  new.geocode.entries$lat[i] <- as.numeric(result[2])
  new.geocode.entries$geoAddress[i] <- as.character(result[3])
}
#merge into exisiting unique cities first and then into gg.geocode
unique.cities <- rbind(new.geocode.entries, unique.cities)
gg.geocode <- left_join(gg.data, unique.cities, by="geocode.value")
gg.geocode$year <- 1983


combined.data <- read.csv("geocoded_data.csv")
combined.data <- combined.data %>% filter(year == 1983)

#keep Entity.Type, mgg.type from combined LC & MGG data. keep type and star.type from GG.
gg.geocode$mgg.type <- NA
gg.geocode$Entity.Type <- NA
combined.data$type <- NA
combined.data$star.type <- NA


combined.data.names <- names(combined.data)
gg.data.names <- names(gg.geocode)

common_cols <- intersect(combined.data.names, gg.data.names) 
print("Column names that are the same in both dataframes:") 
print(common_cols)

all.data <- merge(combined.data, gg.geocode, by = common_cols, all = TRUE)
write.csv(all.data, "alldata.csv")

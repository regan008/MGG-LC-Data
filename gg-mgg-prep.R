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
for(i in 1:nrow(unique_cities)) {
  # Print("Working...")
  result <- tryCatch(geocode(new.geocode.entries$geocode.value[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
  new.geocode.entries$lon[i] <- as.numeric(result[1])
  new.geocode.entries$lat[i] <- as.numeric(result[2])
  new.geocode.entries$geoAddress[i] <- as.character(result[3])
}
unique.cities <- rbind(new.geocode, unique.cities)
gg.geocode <- left_join(gg.data, unique.cities, by="geocode.value")


lc.data <- lc.data %>% select(-"Other.Entities", -"Content.Type", -"Vol..", -"notes", -"Issue..", -"Month", -"ID", -"concat_key", -"orig.lc.lat", -"orig.lc.long")
lc.data$star.type <- "NA"
lc.data <- lc.data %>% select(-"notes")
gg.geocode$amenityfeatures <- "NA"
gg.geocode$Entity.Type <- "NA"
gg.geocode$Year <- 1983
#left off here trying to merge the two dfs. Mandy, comment and clean up the code. 

geocoded.data <- read.csv("geocoded_data.csv")
lc.data <- geocoded.data %>% filter(publication == "Lesbian Connection") %>% filter(year == 1983)
mgg.data <- geocoded.data %>% filter(publication == "Bob Damron's Address Book") %>% filter(year == 1983)


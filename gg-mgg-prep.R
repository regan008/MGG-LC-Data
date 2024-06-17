library(tidyverse)
library(forcats)
library(ggmap)
library(ggplot2)

google_key <- readline(prompt="Please enter your Google API key: ")
print(google_key)

gg.geocode.function <- function (year, google_key) {
  gg.filename <- paste("GG-Data/gg-", year, ".csv", sep = "")
  print(paste("Reading in ", gg.filename, sep = ""))
  
  #load data and add a publication and country column to match the other datasets
  gg.data <- read.csv(gg.filename, header = TRUE)
  gg.data$publication <- "Gaia's Guide"
  gg.data$country <- "United States"
  
  #create a geocode value from the city state and country
  gg.data$geocode.value <-  paste(gg.data$city, ", ", gg.data$state, ", ", gg.data$country, sep="")
  
  # read in a list of unique cities (geocode values) from Damron and LC data. Join it with Gaia's guide data.
  unique.cities <- read.csv("unique_city_list.csv")
  gg.geocode <- left_join(gg.data, unique.cities, by="geocode.value")
  
  #find all the empty rows that didn't have a match. 
  gg.geocode.empty <- gg.geocode %>% filter(is.na(lon))
  new.geocode.entries <<- unique(gg.geocode.empty$geocode.value) %>% as.data.frame() %>% rename("geocode.value" = ".")
  print(paste(length(new.geocode.entries$geocode.value), " entries unmatched in unique values. Will now be geocoded.", sep = ""))
  
  #Register api key and geocode entries with no entry in unique city list
 # register_google(key = Sys.getenv("GOOGLE_KEY"))
  register_google(key = google_key)
  #geocoding function
  for(i in 1:nrow(new.geocode.entries)) {
    # Print("Working...")
    result <- tryCatch(geocode(new.geocode.entries$geocode.value[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
    new.geocode.entries$lon[i] <- as.numeric(result[1])
    new.geocode.entries$lat[i] <- as.numeric(result[2])
    new.geocode.entries$geoAddress[i] <- as.character(result[3])
  }
  print(colnames(new.geocode.entries))
  print(colnames(unique.cities))
  #merge into existing unique cities first and then into gg.geocode. Write new unique cities to csv list as long as they didn't return an NA value
  unique.cities <- rbind(new.geocode.entries[!is.na(new.geocode.entries$lon), ], unique.cities)
  write.csv(unique.cities, "unique_city_list.csv", row.names = FALSE)
  
  gg.geocode <<- left_join(gg.data, unique.cities, by="geocode.value")
  if (!"year" %in% colnames(gg.geocode)){ gg.geocode$year <<- year }
  write.csv(gg.geocode, paste("GG-Data/gg-geocoded-", year, ".csv", sep = ""),row.names = FALSE)
  } #END FUNCTION


# Get a list of files in the GG-Data subfolder that match the "gg-XXXX.csv" pattern
files <- list.files(path = "GG-Data", pattern = "gg-\\d{4}\\.csv$", full.names = FALSE)
years <- gsub("gg-(\\d{4})\\.csv", "\\1", files) # Extract the year from each filename
years <- as.numeric(years) # Convert the years to numeric
completed_years <- unique(years) # Get a list of unique years
# Loop through each unique year in the subfolder and run the gg.geocode.function
for (year in completed_years) {
  gg.geocode.function(year, google_key)
}

#gg.geocode(1983, google_key)

##Note that a many to many error will occur if this is re-run on a year that has already been run through this function.

merge.data <- function() {
  #source("MGG-Data/mgg-prep.R")
  require(dplyr)
  require(readr)
  #pull.matching.mggdata()
  mgg.data <- read.csv("MGG-Data/mgg-data.csv")
  mgg.data <- mgg.data %>% rename(mgg.type = type)
  unique.cities <- read.csv("unique_city_list.csv")
  mgg.data$geocode.value <-  paste(mgg.data$city, ", ", mgg.data$state, ", ", mgg.data$country, sep="")
  mgg.geocode <- left_join(mgg.data, unique.cities, by="geocode.value")
  
  gg.data <- list.files(path = "GG-Data", pattern = "gg-geocoded-\\d{4}\\.csv$", full.names = TRUE) %>% 
    lapply(read_csv) %>% bind_rows 
  gg.data$mgg.type <- NA
  mgg.geocode$type <- NA
  mgg.geocode$star.type <- NA
  if ("X" %in% colnames(mgg.geocode)) { mgg.geocode <- mgg.geocode %>% select(-X)}
  gg.data <- gg.data %>% select(title, type, mgg.type, star.type, city, state, description, publication, country, geocode.value, lon, lat, year)
  mgg.geocode <- mgg.geocode %>% rename("year" = "Year") 
  mgg.geocode <- mgg.geocode %>% select(title, mgg.type, type, star.type, city, state, description, publication, country, geocode.value, lon, lat, year) 
  alldata <- rbind(mgg.geocode, gg.data)
  combined.data <<- alldata
  write.csv(combined.data, "all-data.csv", row.names = FALSE)
  }
merge.data()





#mgg.L or G only entries
#damron.w.spaces <- all.data %>% filter(publication == "Bob Damron's Address Book") 
#damron.w.spaces <- damron.w.spaces %>% filter(grepl("(G)", damron.w.spaces$amenityfeatures, ignore.case = TRUE) | grepl("(L)", damron.w.spaces$amenityfeatures, ignore.case = TRUE))
#gg.lc.data <- all.data %>% filter(publication == "Lesbian Connection" | publication == "Gaia's Guide")
#all.w.data <- merge(damron.w.spaces, gg.lc.data, by = common_cols, all = TRUE)
#write.csv(all.w.data, "gg-mgg-all-w-data.csv")

relative.data <- function(){
  all.data <- read.csv(file = "all-data.csv")
  
  #calculating relative values of locations
  total.per.year <- alldata %>% group_by(publication, year) %>% summarize(pub.count = n())
  total.per.loc.byyear <- alldata %>% group_by(publication, year, geocode.value, lon, lat) %>% summarize(count = n())
  relative.count <- full_join(total.per.year, total.per.loc.byyear)
  relative.count <<- relative.count %>% mutate(relative.percentage = count/pub.count * 100)
  
  write.csv(relative.count, file="gg-mgg-alldata-relative.csv", row.names = FALSE) 
}
relative.data()

#TO DO: FIX SO THAT THE ALL DATA FUNCTION INCLUDES AMENTIY FEATURES IN DATASET. NECESSARY TO FILTER BY WOMEN'S SPACES. 
relative.w.data <- function() {
all.w.data <- read.csv(file = "all-data.csv")
#calculate relative values for data with just Damron G or L
total.per.year <- all.w.data %>% group_by(publication, year) %>% summarize(pub.count = n())
total.per.loc.byyear <- all.w.data %>% group_by(publication, year, city, state, country, geocode.value, lon, lat, geoAddress) %>% summarize(count = n())

relative.count <- full_join(total.per.year, total.per.loc.byyear)
relative.count <- relative.count %>% mutate(relative.percentage = count/pub.count * 100)
write.csv(relative.count, file="gg-mgg-relative-all-w-data.csv", row.names = FALSE) #use this for just women's data
}

ranked.data <- function(){
relative.count <- read.csv(file = "gg-mgg-alldata-relative.csv")
## Generate Ranked count for data (using all of damron)
rank <- relative.count %>%  group_by(year, publication) %>% mutate(rank = rank(-count, ties.method = 'min'))
write.csv(rank, "ranked-data.csv")
}
ranked.data()

map.ranked.cites <- function(pubyear) {
usa <- map_data("state")
rank <- read.csv(file = "ranked-data.csv") %>% filter(year == pubyear)
ggplot() + 
  geom_map( data = usa, map = usa, aes(long, lat, map_id=region)) + borders("state", fill = "white", colour = "grey80") +
  geom_point(data = rank, mapping = aes(x=lon, y=lat, color = publication)) + facet_wrap(~publication, nrow = 3)
}     
map.ranked.cites(1981)

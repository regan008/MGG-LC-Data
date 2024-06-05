create.unique.city.list <- function() {
  require(tidyverse)
  require(ggmap)
  register_google(key = google_key)
  mgg.data <- readRDS("MGG-Data/mgg-data-cleaned.rds")
  mgg.data <- mgg.data %>% filter(state != "GU") %>% filter(state != "Mexico") %>% filter(state != "VI")
  mgg.data$country <- "United States"
  mgg.data$geocode.value <-  paste(mgg.data$city, ", ", mgg.data$state, ", ", mgg.data$country, sep="")
  new.geocode.entries <- unique(mgg.data$geocode.value) %>% as.data.frame() %>% rename(geocode.value = ".")
  for(i in 1:nrow(new.geocode.entries)) {
    # Print("Working...")
    result <- tryCatch(geocode(new.geocode.entries$geocode.value[i], output = "latlona", source = "google"), warning = function(w) data.frame(lon = NA, lat = NA, address = NA))
    new.geocode.entries$lon[i] <- as.numeric(result[1])
    new.geocode.entries$lat[i] <- as.numeric(result[2])
    new.geocode.entries$geoAddress[i] <- as.character(result[3])
  }
  write.csv(new.geocode.entries, "unique_city_list.csv")
}
create.unique.city.list()



pull.matching.mggdata <- function(){
  mgg <- readRDS("MGG-Data/mgg-data-cleaned.rds")
  mgg <- mgg %>% filter(state != "GU") %>% filter(state != "Mexico") %>% filter(state != "VI")
  
  files <- list.files(path = "GG-Data", pattern = "gg-\\d{4}\\.csv$", full.names = FALSE)
  years <- gsub("gg-(\\d{4})\\.csv", "\\1", files) # Extract the year from each filename
  years <- as.numeric(years) # Convert the years to numeric
  completed_years <- unique(years) # Get a list of unique years
  mgg <- mgg %>% filter(Year %in% completed_years)
  write.csv(mgg, "MGG-Data/mgg-data.csv")
}
pull.matching.mggdata()

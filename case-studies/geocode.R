library(tidyverse)
library(forcats)
library(ggmap)
library(ggplot2)
# getting google API key and registering with the service
getGoogleAPI <- function() {
    google_key <- readline(prompt = "Please enter your Google API key: ")
    print(google_key)
    register_google(key = google_key)
}
getGoogleAPI()

setwd(dir = "~/MGG-LC-Data/case-studies" ) ##change as necessary
data <- read.csv("clear-ggdata.csv") 

geocoding_function <- function(GGgeodata) {
    # Trim whitespace from character columns
    GGgeodata <- GGgeodata %>% mutate_if(is.character, trimws)

    # Create a full address by concatenating street address, city, and state
    GGgeodata$full.address <- paste(GGgeodata$address, GGgeodata$city, GGgeodata$state, sep = ", ")
            
    # Initialize columns for longitude, latitude, and geocoded address
    GGgeodata$lon <- NA
    GGgeodata$lat <- NA
    GGgeodata$geoAddress <- NA

    # Print the first 10 full addresses for debugging
    print(GGgeodata$full.address[1:10])

    # Iterate through each row of the data frame
    for (i in 1:nrow(GGgeodata)) {
        # Attempt to geocode the full address, handle errors by returning NA values
        result <- tryCatch(
            {
                res <- geocode(GGgeodata$full.address[i], output = "latlona", source = "google")
                if (nrow(res) == 0) {
                    data.frame(lon = NA, lat = NA, address = NA)
                } else {
                    res
                }
            },
            error = function(e) {
                data.frame(lon = NA, lat = NA, address = NA)
            }
        )
        
        # Check if the result has the expected columns
        if (ncol(result) >= 2) {
          GGgeodata$lon[i] <- as.numeric(result$lon)
          GGgeodata$lat[i] <- as.numeric(result$lat)
          if (ncol(result) >= 3) {
            GGgeodata$geoAddress[i] <- as.character(result$address)
          } else {
            GGgeodata$geoAddress[i] <- NA
          }
        } else {
          GGgeodata$lon[i] <- NA
          GGgeodata$lat[i] <- NA
          GGgeodata$geoAddress[i] <- NA
        } # I added this chunk from the geocode.R file in the MGG-LC-Data/case-studies repo. it seems like it makes sure the address column is stored as "character" in the resulted df which was causing an issue before.
        
        # Print the result for debugging purposes
        print(paste("Result: ", toString(result)))
    }
    return(GGgeodata)
}

#  # Filter out rows where geocoding failed 
GGgeodata <- GGgeodata %>% filter(!is.na(lon))

data.geocoded <- geocoding_function(data)



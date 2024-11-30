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

data <- read.csv("case-studies/clear-ggdata.csv")

geocoding_function <- function(mgg.geocode.data) {
    # Trim whitespace from character columns
    mgg.geocode.data <- mgg.geocode.data %>% mutate_if(is.character, trimws)

    # Create a full address by concatenating street address, city, and state
    mgg.geocode.data$full.address <- paste(mgg.geocode.data$streetaddress, mgg.geocode.data$city, mgg.geocode.data$state, sep = ", ")

    # Initialize columns for longitude, latitude, and geocoded address
    mgg.geocode.data$lon <- NA
    mgg.geocode.data$lat <- NA
    mgg.geocode.data$geoAddress <- NA

    # Print the first 10 full addresses for debugging
    print(mgg.geocode.data$full.address[1:10])

    # Iterate through each row of the data frame
    for (i in 1:nrow(mgg.geocode.data)) {
        # Attempt to geocode the full address, handle errors by returning NA values
        result <- tryCatch(
            {
                res <- geocode(mgg.geocode.data$full.address[i], output = "latlona", source = "google")
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
            mgg.geocode.data$lon[i] <- as.numeric(result$lon)
            mgg.geocode.data$lat[i] <- as.numeric(result$lat)
            if (ncol(result) >= 3) {
                mgg.geocode.data$geoAddress[i] <- as.character(result$address)
            } else {
                mgg.geocode.data$geoAddress[i] <- NA
            }
        } else {
            mgg.geocode.data$lon[i] <- NA
            mgg.geocode.data$lat[i] <- NA
            mgg.geocode.data$geoAddress[i] <- NA
        }

        # Print the result for debugging purposes
        print(paste("Result: ", toString(result)))
    }

    # Filter out rows where geocoding failed (lon is NA)
    failed_geocode <- mgg.geocode.data %>% filter(is.na(lon))

    # Save the failed geocoding attempts to a CSV file
    write.csv(failed_geocode, "case-studies/failed-geocode.csv", row.names = FALSE)

    # Return the updated data frame with geocoding results
    return(mgg.geocode.data)
}
mgg.data.geocoded <- geocoding_function(data)

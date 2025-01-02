library(tidyverse)
library(forcats)
library(ggmap)
library(ggplot2)
library(dplyr)
# getting google API key and registering with the service
getGoogleAPI <- function() {
  google_key <- readline(prompt = "Please enter your Google API key: ")
  print(google_key)
  register_google(key = google_key)
}
getGoogleAPI()

#setwd("Downloads/") changed directories to run the script locally in my laptop
setwd("~/MGG-LC-Data/case-studies")

#Read clear data

data <- read.csv("clear-ggdata.csv") # change this as needed

geocoding_function <- function(GGgeodata) {
  # Trim whitespace from character columns
  GGgeodata <- GGgeodata %>% mutate_if(is.character, trimws)
  
  # Create a full address by concatenating street address, city, and state
  GGgeodata$full.address <- paste(GGgeodata$address, GGgeodata$city, GGgeodata$state, sep = ", ")
  # your clear-ggdata.csv doesn't have a column named streetaddress, it's only "address". I changed this line to reflect that
  
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


cleardata.geocoded <- geocoding_function(data)


##Read unclear data###


uncleardata <- read.csv("unclear-ggdata.csv") # change this as needed


geocoding_function <- function(GGgeodata) {
  # Trim whitespace from character columns
  GGgeodata <- GGgeodata %>% mutate_if(is.character, trimws)
  
  # Create a full address by concatenating street address, city, and state
  GGgeodata$full.address <- paste(GGgeodata$address, GGgeodata$city, GGgeodata$state, sep = ", ")
  # your clear-ggdata.csv doesn't have a column named streetaddress, it's only "address". I changed this line to reflect that
  
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

uncleardata.geocoded <- geocoding_function(uncleardata)

#combine dataframes

combined_GGdata <- rbind(cleardata.geocoded, uncleardata.geocoded)


#add MGG data
MGdata <- read_csv("~/MGG-LC-Data/MGG-Data/data.csv")

#add publication column
MGdata <- MGdata %>%
  mutate(publication = "Damron's Guide")

#rename address column
MGdata <- MGdata %>%
  rename(address = streetaddress)

#filter for case study cities
states_to_include <- c("TX", "MI", "OR")  
cities_to_include <- c("Dallas", "Detroit", "Portland") 

filtered_MGG <- MGdata %>%
  filter(state %in% states_to_include & city %in% cities_to_include)

#combine dataframes
combined_MGG_GG <- bind_rows(combined_GGdata, filtered_MGG)

#filter out unclear addresses
filtered_MGG_GG <- combined_MGG_GG %>%
  filter(!is.na(address))


# Save dataset as an RDS file
saveRDS(filtered_MGG_GG, "~/MGG-LC-Data/case-studies/filtered_MGG_GG.rds")




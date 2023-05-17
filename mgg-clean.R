library(tidyverse)
mgg.data <- readRDS(file = "mgg-data.rds")
mgg.data <- mgg.data %>% select(-ID, -streetaddress, -lat, -lon)

mgg.data <- mgg.data %>% mutate_if(is.character, trimws)
mgg.data.notfiltered <- mgg.data %>% filter(state != "CA" & state != "NY" & state != "FL" & state != "IL")
mgg.ca <- mgg.data %>% filter(state == "CA")
mgg.ca <- mgg.ca %>% mutate(city = fct_collapse(mgg.ca$city, 
                                                Alleghany = "Alleghanny",
                                                Clearlake =	"Clear Lake",
                                                "Clearlake Oaks" = "Clear Lake Oaks",
                                                "Culver City"	= "Culber City",
                                                "El Monte"	= "South El Monte",
                                                Hawthorne	= "Hawthrone",
                                                Laguna	= "South Laguna",
                                                "Lake Tahoe"	= "South Lake Tahoe",
                                                Lompoc	= "Lampoc",
                                                "Los Angeles"	= c("Highland Park", "Hollywood", "Hollywood Station", "Hollywood West", "Midtown", "Midtown- Los Angeles", "N. Hollywood", "North Hollywood", "Silver Lake", "Silverlake", "Studio City", "Valley", "Van Nuys", "West Hollywood", "West Los Angeles"),
                                                Pasadena = "South Pasadena",
                                                Philipsville = "Philipsville, Humbolt County", 
                                                "Redwood City" = "Redwood CIty",
                                                "Russian River"	= "Russian RIver",
                                                Sacramento	= c("South Sacramento", "West Sacramento"),
                                                "San Francsico"	= c("Castro Village", "Castro Village- S.F.", "Folsom", "Folsom- S.F.", "Haight, Filmore & West- S.F.", "Mission District","Polk Street Area", "Polk Street Area- S.F.", "Tenderloin", "Tenderloin to North Beach- S.F."),
                                                "San Gabriel"	= "South San Gabriel",
                                                Tahoe	= "Tahoe Vista"))
  
  
mgg.fl <- mgg.data %>% filter(state == "FL")
mgg.fl <- mgg.fl %>% mutate(city = fct_collapse(mgg.fl$city,
                                              "Daytona Beach"	= "S. Daytona Beach",
                                              Dunedin =	"Dunepin",
                                              "Ft. Lauderdale"	= c("Ft. Lauderdale Beach",	"Ft. Launderdale"),
                                              Gainesville	= "Gainsville",
                                              Hialeah	= "Hilaleah",
                                              Jacksonville	= "Jacksonville Beach",
                                              "Madeira Beach" =	"Medeira Beach",
                                              Miami	= "North Miami",
                                              "Miami Beach"	= "North Miami Beach",
                                              "Pinellas Park" =	c("Pineallis Park", "Pinellis Park",
                                                                  "Ponce de Leon"	= "Ponce De Leon")))
        

mgg.il <- mgg.data %>% filter(state == "IL")
mgg.il <- mgg.il %>% mutate(city = fct_collapse(mgg.il$city,
                                                Carbondale = "Carbondle",
                                                Chicago	= c("Chicago Heights", "Chicago- Near North Side", "Chicago- New Town", "Chicago-North Side", "Chicago-South Side"),
                                                "Des Plaines" =	"Des Plains",
                                                "Elk Grove Village" =	"Elk Grove",
                                                Hinsdale = "Hindale"))


mgg.ny <- mgg.data %>% filter(state == "NY")
mgg.ny <- mgg.ny %>% mutate(city = fct_collapse(mgg.ny$city,
                                              Babylon	= c("Babylon, L.I", "Babylon, L.I."),
                                              Baldwin	= "Baldwin, L.I.",
                                              "Bay Shore" = "Bayshore",
                                              Bayville = "Bayville, L.I.",
                                              Bellmore = c("Bellmore, L.I.", "N. Bellmore"),
                                              Binghamton = "Binghampton",
                                              Bronx	= "The Bronx",
                                              Brooklyn = "Brooklyn Heights",
                                              Copiague = c("Copaigue", "Copiaque"),
                                              "Croton Falls" = "Crofton Falls",
                                              "Deer Park"	= c("Deep Park", "Deer Park, L.I."),
                                              "East Hampton" = "Easthampton",
                                              "East Meadow"	= "E. Meadow",
                                              "East Northport" =	"E. Northport",
                                              "Fire Island" =	"Fire Island, L.I.",
                                              "Floral Park"	= "Floral Park, L.I.",
                                              "Hauppauge"	= "Hauppauge, L.I.",
                                              "Huntington" = "Huntington, L.I.",
                                              "Lake Ronkonkoma"	= c("Lake Ronkonkoma, L.I.", "Lake Ronkonnoma"),
                                              "New York City" =	c("Chelsea", "Greenwich Village", "Greenwich Village & Chelsea", "Manhattan", "Midtown", "New York", "Uptown", "Uptown- N.Y.C."),
                                              "Niagra Falls" = "Niagara Falls",
                                              "North Bellmore" = "North Bellmore, L.I.",
                                              "Northport"	= "Northpoint",
                                              Queens = c("Forest Hill", "Forest Hills", "Forrest Hill", "Jackson Heights"),
                                              Sayville = "Sayville, L.I.",
                                              Smithtown = "Smithtown, L.I.",
                                              Southampton = c("South Hampton", "Southhampton"),
                                              Utica = "N. Utica",
                                              Wantagh	= "Wantagh, L.I.",
                                              "West Hempstead" = "West Hempstead, L.I.",
                                              Westbury = "Westbury, L.I."
                                              ))
mgg.data <- rbind(mgg.data.notfiltered, mgg.il, mgg.ny, mgg.fl, mgg.ca)
mgg.data$publication <- c("Bob Damron's Address Book")
mgg.data$country <- c("United States")

saveRDS(mgg.data, file="mgg-data-cleaned.rds")

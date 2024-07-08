library(tidyverse)
mgg.gg.data <- read.csv(file = "final-output-data/relative-location-data-all-years.csv")

full.data <- read.csv(file = "final-output-data/all-data-cleaned-geocoded.csv")

full.data <- full.data %>% filter(!grepl(",Cruising Areas", full.data$type, fixed = TRUE) & 
                            !grepl("Cruising Areas,", full.data$type, fixed = TRUE) & 
                            !grepl("Cruising Areas", full.data$type, fixed = TRUE) & 
                            !grepl("Cruisy Areas", full.data$type, fixed = TRUE) &
                            !grepl("Crusiy Areas", full.data$type, fixed = TRUE))

mgg.data <- mgg.gg.data %>% filter(publication == "Bob Damron's Address Book")
gg.data <- mgg.gg.data %>% filter(publication == "Gaia's Guide")

gg.data <- gg.data  %>% mutate(uniqueID = paste("gg.", row_number(), sep=""))
mgg.data <- mgg.data  %>% mutate(uniqueID = paste("mgg.", row_number(), sep=""))

unique_years_string <- full.data %>% 
  select(year) %>% 
  distinct() %>% 
  arrange(year) %>% 
  pull(year) %>% 
  paste(collapse = ", ")

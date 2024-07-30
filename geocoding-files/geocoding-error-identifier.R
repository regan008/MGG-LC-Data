library(tidyverse)
data <- read.csv("final-output-data/all-data-cleaned-geocoded.csv")
data <- data %>% filter(is.na(lat))
data <- data %>%
    group_by(publication, year, geocode.value) %>%
    summarize(count = n())
write.csv(data, "~/desktop/geocode-errors.csv", row.names = FALSE)


setwd("~/MGG-LC-Data")
library(dplyr)

MGdata <- read_csv("geocoded_data.csv")

MGdata <- MGdata %>%
  mutate(X = row_number())

MGData <- MGData %>%
  mutate(X = as.integer(X))

combined_GGdata <- combined_GGdata %>%
  mutate(X = as.integer(X))

#combine dataframes
combined_MGG_GG <- bind_rows(combined_GGdata, MGdata)
library(tidyverse)
mgg.data <- readRDS(file = "mgg-data.rds")
mgg.data <- mgg.data %>% select(-ID, -streetaddress, -lat, -lon)



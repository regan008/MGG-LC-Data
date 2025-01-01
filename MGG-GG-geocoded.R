
setwd("~/MGG-LC-Data")
library(dplyr)

MGdata <- read_csv("~/MGG-LC-Data/MGG-Data/data.csv")


#combine dataframes
combined_MGG_GG <- bind_rows(combined_GGdata, MGdata)
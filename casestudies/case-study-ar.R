library(tidyverse)
library(ggmap)

csl <- read.csv("case-studies/case_study_locations.csv")


# Merge the two year columns into one
csl <- csl %>%
    mutate(year.r = coalesce(year, Year)) %>%
    select(-year, -Year) %>%
    rename(year = year.r)

# Dallas
d.csl <- csl %>% filter(city == "Dallas")
d.csl.75 <- d.csl %>% filter(year == 1977)

register_google(Sys.getenv("MGG_GOOGLE_KEY"))
dallas_map <- get_map(location = "Dallas, Texas", zoom = 12, maptype = "roadmap")

# Plot the map with locations from d.csl.75, colored by publication
ggmap(dallas_map) +
    geom_point(
        data = d.csl.75,
        aes(x = lon, y = lat, color = publication), # Map color to publication
        size = 3,
        alpha = 0.7
    ) +
    labs(
        title = "Locations in Dallas (1975) by Publication",
        x = "Longitude",
        y = "Latitude",
        color = "Publication" # Legend title
    )

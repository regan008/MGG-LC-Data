## Initialize an empty dataframe to store all data
empty.df <- data.frame(
    title = character(),
    description = character(),
    type = character(),
    address = character(),
    city = character(),
    state = character(),
    country = character(),
    publication = character(),
    year = numeric(),
    notes = character(),
    unclear.address = character(),
    stringsAsFactors = FALSE # This ensures that string data does not get converted to factors
)
columns <- colnames(empty.df)
years <- c(1975, 1977, 1979, 1981, 1983, 1985, 1987, 1989) ## CHANGE THIS TO ADD MORE YEARS AS NEEDED DEPENDING ON WHICH DATA IS COMPLETE

## Read in Gaia's Guide data
load_gg_data <- function(df, columns, years) {
    # Get a list of files in the GG-Data subfolder that match the "gg-XXXX.csv" pattern
    # files <- list.files(path = "GG-Data", pattern = "gg-\\d{4}\\.csv$", full.names = FALSE)
    # years <- gsub("gg-(\\d{4})\\.csv", "\\1", files) # Extract the year from each filename
    # years <- as.numeric(years) # Convert the years to numeric
    # years <- unique(years) # Get a list of unique years

    # Loop through each unique year in the subfolder to open them
    for (year in years) {
        gg.filename <- file.path("GG-Data", paste("gg-", year, ".csv", sep = ""))
        print(paste("Reading in ", gg.filename, sep = ""))
        # load data and add a publication and country column to match the other datasets
        gg.data <- read.csv(gg.filename, header = TRUE)
        gg.data$publication <- "Gaia's Guide"
        gg.data$country <- "United States"
        gg.data <- select(gg.data, intersect(columns, names(gg.data)))
        df <- rbind(df, gg.data)
    }
    return(df)
}
gg.data <- load_gg_data(empty.df, columns, years)

gg.data <- gg.data %>% filter(city == "Detroit" | city == "Dallas" | city == "Portland" & state == "OR")

unclear.gg.data <- gg.data %>% filter(unclear.address == "checked")

write.csv(unclear.gg.data, "case-studies/unclear-ggdata.csv")

clear.gg.data <- gg.data %>% filter(unclear.address != "checked")
write.csv(clear.gg.data, "case-studies/clear-ggdata.csv")

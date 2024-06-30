# MGG-LC-Data

New Datasets:

- Contained in `final-output-data` folder
- `all-data-cleaned-geocoded.csv` - full list of records in both MGG and GG for specific years (that have been completed for GG) - ex. an individual business that has been transcribed, cleaned, and geocoded
- `relative-location-data-all-years.csv` - aggregating all of the records together for MGG and then for GG (years get collapsed and treated as a single set of data), then calculating the relative frequency with which individual place names were mentioned by that publication as a percentage of all place mentions
- `relative-location-data-by-year.csv` - relative frequency of an individual location, per publication, within a given year (ex. Gaia's Guide mentioned New York 100 times in 1981 and all places were mentioned a total of 1,000 times, so relative frequency ofNew York is 10%)
- `ranked-locations-all-years.csv` - ranking of locations based on their relative frequency in a given publication across all given years
- `ranked-locations-by-year.csv` - ranking of locations based on their relative frequency in a given publication for a given years

Old Datasets:

1. `gg-mgg-all-data.csv` - All MGG and GG Data.
2. `gg-mgg-all-w-data.csv` - All data from GG and data that was classified as either (W), (G), or (L) within the Damron Guides.
3. `gg-mgg-alldata-relative.csv` - All MGG and GG Data by publication and city for each year. Relative percentage is the count of the number of locations within that city divided by the total locations in that year of the publication. For example, Anchorage, Alaska had 12 locations in the 1983 Damron Guide. Dividing this number by the total locations in the 1983 guides gives us a relative percentage of 22.65.
4. `gg-mgg-relative-all-w-data.csv` - Same relative percentage as described above but for only the locations which Damron classified as (W), (G), or (L).

library(tidyverse)
library(sf)
library(spdep)
library(ggplot2)
library(scales)

# Load COVID-19 data
covid_data <- read_csv("data/USCounties_cases.csv")

# Filter to contiguous 48 states (exclude AK, HI, and territories)
contiguous_states <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DE", "FL", "GA", 
                      "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                      "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", 
                      "NJ", "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", 
                      "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", 
                      "WV", "WI", "WY", "DC")

covid_filtered <- covid_data %>%
  filter(ST_Abbr %in% contiguous_states) %>%
  filter(!is.na(FatalityRa)) %>%
  mutate(FIPS = str_pad(as.character(FIPS), 5, pad = "0"))

# Download US counties shapefile from the US Census
url <- "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_20m.zip"
temp_zip <- tempfile(fileext = ".zip")
temp_dir <- tempdir()

download.file(url, temp_zip)
unzip(temp_zip, exdir = temp_dir)

# Read the shapefile
counties_sf <- st_read(file.path(temp_dir, "cb_2020_us_county_20m.shp")) %>%
  filter(STATEFP %in% c("01", "04", "05", "06", "08", "09", "10", "12", "13", 
                        "16", "17", "18", "19", "20", "21", "22", "23", "24", 
                        "25", "26", "27", "28", "29", "30", "31", "32", "33", 
                        "34", "35", "36", "37", "38", "39", "40", "41", "42", 
                        "44", "45", "46", "47", "48", "49", "50", "51", "53", 
                        "54", "55", "56", "11"))

# Join COVID data with spatial data
counties_covid <- counties_sf %>%
  left_join(covid_filtered, by = c("GEOID" = "FIPS")) %>%
  filter(!is.na(FatalityRa))

# Create spatial weights matrix (Queen contiguity)
coords <- st_coordinates(st_centroid(counties_covid))
nb <- poly2nb(counties_covid, queen = TRUE)
lw <- nb2listw(nb, style = "W", zero.policy = TRUE)

# Calculate Local Moran's I
local_moran <- localmoran(counties_covid$FatalityRa, lw, zero.policy = TRUE)

# Add Local Moran's I results to the data
counties_covid <- counties_covid %>%
  mutate(
    local_i = local_moran[,1],
    pvalue = local_moran[,5],
    zscore = local_moran[,4],
    # Classify outliers based on significance and type
    outlier_type = case_when(
      pvalue > 0.05 ~ "Not Significant",
      local_i > 0 & pvalue <= 0.05 ~ "High-High Cluster",
      local_i < 0 & pvalue <= 0.05 & 
        FatalityRa > mean(FatalityRa, na.rm = TRUE) ~ "High-Low Outlier",
      local_i < 0 & pvalue <= 0.05 & 
        FatalityRa < mean(FatalityRa, na.rm = TRUE) ~ "Low-High Outlier",
      TRUE ~ "Low-Low Cluster"
    )
  )

# Create the map
map_plot <- ggplot(counties_covid) +
  geom_sf(aes(fill = outlier_type), color = "white", size = 0.1) +
  scale_fill_manual(
    values = c(
      "Not Significant" = "lightgray",
      "High-High Cluster" = "#d73027",
      "Low-Low Cluster" = "#1a9850",
      "High-Low Outlier" = "#f46d43",
      "Low-High Outlier" = "#74add1"
    ),
    name = "Local Moran's I\nSpatial Pattern"
  ) +
  theme_void() +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8),
    plot.title = element_text(size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5)
  ) +
  labs(
    title = "Local Moran's I Analysis of COVID-19 Fatality Rates",
    subtitle = "US Counties (Contiguous 48 States)",
    caption = "High-High: High fatality rate clusters | Low-Low: Low fatality rate clusters\nHigh-Low: High fatality outliers | Low-High: Low fatality outliers"
  ) +
  coord_sf(crs = st_crs(3857), expand = FALSE)

# Save the map
ggsave("output/map.png", map_plot, 
       width = 12, height = 8, dpi = 300, units = "in")

# Print summary statistics
cat("Local Moran's I Analysis Summary:\n")
cat("================================\n")
summary_stats <- counties_covid %>%
  st_drop_geometry() %>%
  count(outlier_type) %>%
  arrange(desc(n))

print(summary_stats)

cat("\nFatality Rate Statistics:\n")
cat("Mean fatality rate:", mean(counties_covid$FatalityRa, na.rm = TRUE), "\n")
cat("Median fatality rate:", median(counties_covid$FatalityRa, na.rm = TRUE), "\n")
cat("Standard deviation:", sd(counties_covid$FatalityRa, na.rm = TRUE), "\n")

cat("\nSignificant spatial patterns (p <= 0.05):", 
    sum(counties_covid$pvalue <= 0.05, na.rm = TRUE), "counties\n")
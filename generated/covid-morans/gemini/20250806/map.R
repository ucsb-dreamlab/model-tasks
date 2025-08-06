# Install packages if not already installed
if (!require("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
}
if (!require("sf", quietly = TRUE)) {
  install.packages("sf", repos = "http://cran.us.r-project.org")
}
if (!require("tigris", quietly = TRUE)) {
  install.packages("tigris", repos = "http://cran.us.r-project.org")
}
if (!require("spdep", quietly = TRUE)) {
  install.packages("spdep", repos = "http://cran.us.r-project.org")
}
if (!require("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2", repos = "http://cran.us.r-project.org")
}


# Load libraries
library(tidyverse)
library(sf)
library(tigris)
library(spdep)
library(ggplot2)

# Load COVID-19 data
covid_data <- read_csv("https://dreamlab-public.s3.us-west-2.amazonaws.com/ocfl/model-tasks/v1/content/data/USCounties_cases.csv")

# Get county geometry data
counties_sf <- counties(cb = TRUE, resolution = "500k")

# Data wrangling
# Calculate fatality rate
covid_data <- covid_data %>%
  mutate(fatality_rate = DeathsbyPo / Confirmedb)

# Join COVID data with spatial data
counties_sf <- counties_sf %>%
  left_join(covid_data, by = c("GEOID" = "FIPS"))

# Filter for contiguous US and remove NA fatality rates
contiguous_us <- counties_sf %>%
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>%
  filter(!is.na(fatality_rate))

# Define neighbors
neighbors <- poly2nb(contiguous_us)

# Remove counties with no neighbors
has_neighbors <- card(neighbors) > 0
contiguous_us <- contiguous_us[has_neighbors, ]
neighbors <- poly2nb(contiguous_us)


listw <- nb2listw(neighbors, style = "W", zero.policy = TRUE)

# Calculate Local Moran's I
local_moran <- localmoran(contiguous_us$fatality_rate, listw, zero.policy = TRUE)

# Prepare data for plotting
contiguous_us$lisa_p <- local_moran[, "Pr(z != E(Ii))"]
contiguous_us$lisa_i <- local_moran[, "Ii"]
contiguous_us$mean_fatality_rate <- mean(contiguous_us$fatality_rate)

# Create LISA cluster categories
contiguous_us <- contiguous_us %>%
  mutate(lisa_cluster = case_when(
    lisa_p < 0.05 & lisa_i > 0 & fatality_rate >= mean_fatality_rate ~ "High-High",
    lisa_p < 0.05 & lisa_i > 0 & fatality_rate < mean_fatality_rate ~ "Low-Low",
    lisa_p < 0.05 & lisa_i < 0 & fatality_rate >= mean_fatality_rate ~ "High-Low",
    lisa_p < 0.05 & lisa_i < 0 & fatality_rate < mean_fatality_rate ~ "Low-High",
    TRUE ~ "Not significant"
  ))

# Create the map
map <- ggplot(contiguous_us) +
  geom_sf(aes(fill = lisa_cluster), color = "white", size = 0.1) +
  scale_fill_manual(values = c(
    "High-High" = "red",
    "Low-Low" = "blue",
    "High-Low" = "pink",
    "Low-High" = "lightblue",
    "Not significant" = "grey"
  )) +
  labs(
    title = "Local Moran's I Outliers for COVID-19 Fatality Rates",
    subtitle = "Contiguous US Counties",
    fill = "LISA Cluster"
  ) +
  theme_void()

# Save the map
ggsave("map.png", map, width = 12, height = 8, dpi = 300)
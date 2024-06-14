##----------------------------------------------------------------------------##
# 3_arenicola_map
# This script contains code to plot a map of the distribution of species
##----------------------------------------------------------------------------##

# 0. PACKAGES ------------------------------------------------------------------
library(here)
library(dplyr)
library(janitor)
library(stringr)
library(terra)
library(sf)
library(ggplot2)
library(ggpubr)

# 1. LOAD DATA -----------------------------------------------------------------

## 1.1. Clean data -------------------------------------------------------------

# Read data in
arenicola <- read.csv(here("data", "raw_data", "Arenicola_Table1supppl.csv"),
                               header = TRUE, sep = ";")

# Check data
glimpse(arenicola)

# Fix column names
arenicola <- arenicola |>
  clean_names()

# Check values for depth
levels(as.factor(arenicola$depth_m))

# Write function to replace values contain "-" with the larger number
replace_depth_values <- function(value) {
  if (value == "" | value == "-") {
    return(NA)
  } else if (grepl("-", value)) {
    # split the string by "-" and take the maximum value
    parts <- unlist(strsplit(value, "-"))
    return(max(as.numeric(parts), na.rm = TRUE))
  } else {
    return(as.numeric(value))
  }
}

# Apply function to column depth
arenicola_clean <- arenicola %>%
  mutate(depth_m    = sapply(depth_m, replace_depth_values))

# Check if it worked
levels(as.factor(arenicola_clean$depth_m)) #looks ok

## 1.2. Get Norway shapefile ---------------------------------------------------

## 1.3. Convert occurrences to spatial df --------------------------------------

# Check column names
colnames(arenicola_clean)

# Convert the 
arenicola_sf <- st_as_sf(arenicola_clean,coords=c("decimalLongitude","decimalLatitude"),crs=crs(globalbiomes))
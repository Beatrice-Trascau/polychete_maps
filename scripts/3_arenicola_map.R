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
library(plotly)
library(scales)
library(ggspatial)
library(cowplot)

# 1. LOAD DATA -----------------------------------------------------------------

## 1.1. Clean data -------------------------------------------------------------

# Read data in
arenicola <- read.csv(here("data", "raw_data", "Table1_submit.csv"),
                               header = TRUE, sep = ";")

# Check data
glimpse(arenicola)

# Fix column names
arenicola <- arenicola |>
  clean_names()

# Check values for depth
levels(as.factor(arenicola$depth_m))

# Check species names
unique(arenicola$x_species) # spaces at the ends of names - need to be removed

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
  mutate(depth_m    = sapply(depth_m, replace_depth_values),
         x_species = str_trim(x_species)) |>
  # remove rows with species name different from Arenicola marina and Arenicolides ecaudata
  filter(!x_species %in% c("Arenicola sp.", "Arenicolidae"))

# Check if it worked
levels(as.factor(arenicola_clean$depth_m)) #looks ok
unique(arenicola_clean$x_species)

## 1.2. Get Norway shapefile ---------------------------------------------------

norway <- geodata::gadm(country = "NOR", level = 0, 
                        path = tempdir(),
                        version = "latest")
# Check shapefile
plot(norway)

# Convert norway to sf object
norway_sf <- st_as_sf(norway)

## 1.3. Convert occurrences to spatial df --------------------------------------

# Check column names
colnames(arenicola_clean)

# Convert to spatial datafram for plotting
arenicola_sf <- st_as_sf(arenicola_clean,
                         coords = c("longitude","latitude"),
                         crs = crs(norway_sf))
# 3. PLOT ----------------------------------------------------------------------  

## 3.1. Plot two separate maps -------------------------------------------------

# Extract depth minimum and maximums
depth_min <- min(arenicola_clean$depth_m, na.rm = TRUE) #0
depth_max <- max(arenicola_clean$depth_m, na.rm = TRUE) #387

# Define additional breaks for legend
additional_breaks <- c(100, 200, 300)

# Combine min, max, and additional breaks
breaks <- sort(c(depth_min, depth_max, additional_breaks))

# Plot map for A. marina
a_marina_df <- arenicola_sf |>
  filter(x_species == "Arenicola marina")

arenicola_marina <- ggplot() +
  geom_sf(data = norway_sf, fill = "lightgray", color = "white") +
  geom_sf(data = a_marina_df, aes(color = depth_m)) +
  annotation_north_arrow(location = "br", which_north = "true",
                         pad_y = unit(0.8, "cm"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "br", width_hint = 0.35) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() +
  labs(color = "Depth (m)",
       title = "Arenicola marina") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.title = element_text(face = "italic"))

# Plot map for A. ecaudata
a_ecaudata_df <- arenicola_sf |>
  filter(x_species == "Arenicolides ecaudata")

arenicolides_ecaudata <- ggplot() +
  geom_sf(data = norway_sf, fill = "lightgray", color = "white") +
  geom_sf(data = a_ecaudata_df, aes(color = depth_m)) +
  scale_color_gradient(low = "blue", high = "red",
                       limits = c(depth_min, depth_max),
                       breaks = breaks,
                       labels = breaks) +
  theme_classic() +
  labs(color = "Depth (m)",
       title = "Arenicolides ecaudata") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = c(0.8, 0.12),
        plot.title = element_text(face = "italic"),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()) 

# Arrange the two plots in a single figure
arenicola_map1 <- plot_grid(arenicola_marina, arenicolides_ecaudata,
                            ncol = 2, align = "hv", rel_heights = c(1, 1))

# Save to file
ggsave(here("figures", "arenicola_map1.png"),
       width=13, height=9)

## 3.2. Plot with facet wrap ---------------------------------------------------
# Plot map with two panels
arenicola_map2 <- ggplot() +
  geom_sf(data = norway_sf, fill = "lightgray", color = "white") +
  geom_sf(data = arenicola_sf, aes(color = depth_m)) +
  facet_wrap(~ x_species) +
  scale_color_gradient(low = "blue", high = "red") +
  theme_classic() +
  labs(color = "Depth (m)") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "bottom")

# Save to file
ggsave(here("figures", "arenicola_map2.png"),
       width=13, height=9)

# END OF SCRIPT ----------------------------------------------------------------
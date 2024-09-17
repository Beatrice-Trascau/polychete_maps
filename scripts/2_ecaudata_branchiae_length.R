##----------------------------------------------------------------------------##
# 2_ecaudata_branchiae_length
# This script contains code to plot the length of different species
##----------------------------------------------------------------------------##

# 0.PACKAGES -------------------------------------------------------------------
library(here)
library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr)

# 1. LOAD DATA -----------------------------------------------------------------

# Read data in
ecaudata_branchiae <- read.csv(here("data", "raw_data", "ecaudata_branchiae.csv"),
                            header = TRUE, sep = ";")

# Check data
glimpse(ecaudata_branchiae)
# length values detected as character because of , instead of .

# Replace , with . in Length column
ecaudata <- ecaudata_branchiae |>
  mutate(Length = as.numeric(gsub(",", ".", gsub("\\.", "", Length))))

# 2. PLOT ----------------------------------------------------------------------

## 2.1. With ggplot ------------------------------------------------------------

# Create plot
ecaudata_plot1 <- ggplot(ecaudata, aes(x = X...Specimen, y = Length)) +
  geom_point(aes(shape = Branchiae), size = 4, 
             position = position_jitter(width = 0.2, height = 0)) +
  scale_shape_manual(values = c("yes" = 16, "no" = 17)) +
  scale_alpha_manual(values = c("yes" = 1, "no" = 0.1)) + 
  ylab("Length (mm)") +
  xlab("Individual") +
  theme_classic() +
  theme(legend.position = "none")

# Save to file
ggsave(here("figures", "ecaudata_plot1.png"),
       width=13, height=9)

## 2.2. With ggpubr ------------------------------------------------------------

# Create plot
ecaudata_plot2 <- ggdotchart(ecaudata, x = "X...Specimen", y = "Length",                              
                             shape = "Branchiae",
                             sorting = "ascending",
                             xlab = "Individual",
                             ylab = "Length (mm)",
                             ggtheme = theme_pubr()) + guides(shape = "none")  

ggsave(here("figures", "ecaudata_plot2.png"),
       width=13, height=9)

# END OF SCRIPT ----------------------------------------------------------------
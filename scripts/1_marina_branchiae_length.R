##----------------------------------------------------------------------------##
# 1_marina_branchiae_length
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
branchia_length <- read.csv(here("data", "raw_data", "marina_branchiae.csv"),
                            header = TRUE, sep = ";")

# Check data
glimpse(branchia_length)
   # length values detected as character because of , instead of .

# Replace , with . in Length column
branchia <- branchia_length |>
  mutate(Length = as.numeric(gsub(",", ".", gsub("\\.", "", Length))),
  # create third column with species name and presence/absence of branchiae       
         speices_branchiae = paste(Species, Branchiae, sep = "_"))
  

# 2. PLOT ----------------------------------------------------------------------

## 2.1. With ggplot ------------------------------------------------------------

# Create plot
branchia_plot1 <- ggplot(branchia, aes(x = Nummer, y = Length)) +
  geom_point(aes(shape = speices_branchiae), size = 4, 
             position = position_jitter(width = 0.2, height = 0)) +
  ylab("Length (mm)") +
  xlab("Individual") +
  theme_classic() +
  theme(legend.position = "none")

# Save to file
ggsave(here("figures", "branchia_plot1.png"),
       width=13, height=9)

## 2.2. With ggpubr ------------------------------------------------------------

# Create plot
branchia_plot2 <- ggdotchart(branchia, x = "Nummer", y = "Length",                              
           shape = "speices_branchiae",
           sorting = "ascending",
           xlab = "Individual",
           ylab = "Length (mm)",
           ggtheme = theme_pubr()) + guides(shape = "none")  

ggsave(here("figures", "branchia_plot2.png"),
       width=13, height=9)

# END OF SCRIPT ----------------------------------------------------------------
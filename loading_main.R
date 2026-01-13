# Main setup and control script for discrete data loading
# Script:  loading_man.R - Broughton LSSM version
# Created: February 2024. EJG
############################################################################----
# This script sources the necessary libraries and functions, coordinates the
# analysis, and creates data structures that are then 'knitted' together (I think).
# So the idea is to run bits in here, and then 'Render' the RMD script.
# Seems straightforward. :)
#
## Updates:
# 2026/01/13: Loads discrete and mooring data, as well as necessary 2025 conditions
#   for growth model including tidal currents and light levels. 
#================================== Load required packages =================================

# check for any required packages that aren't installed and install them
required.packages <- c( "readxl", "readr", "ggplot2", "tidyr", "dplyr", "stringr", "lubridate", "ggtext",
                        "RColorBrewer", "rmarkdown", "knitr", "tinytex", "kableExtra",
                        "patchwork" )

uninstalled.packages <- required.packages[!(required.packages %in% installed.packages()[, "Package"])]

# install any packages that are required and not currently installed
if(length(uninstalled.packages)) install.packages(uninstalled.packages)

# require all necessary packages
lapply(required.packages, require, character.only = TRUE)
#lapply(required.packages, library, character.only = TRUE)
getRversion()

# Clear environment and get today's date (for saving files)
rm(list = ls(all = T))
tooday <- format(Sys.Date(), "%Y-%m-%d")

#======================== Directories and constants ===========================
# Will be created if they don't exist.
source_dir  <- 'C:/Data/Git/LSSM_Water_Analysis/source_data'
results_dir <- 'C:/Data/Git/LSSM_Water_Analysis/Results'


source( 'C:/Data/Git/LSSM_Water_Analysis/loading_functions.r')
# Projections as EPSG codes for when we need to map the sample locations
albers_crs <- 3005 # Or for newer datasets: albers_crs <- 3153
UTM_crs    <- 26909 # For Zone 9N NAD83. Or for WGS84: 32609








outname <- paste0("Broughton_Water_Analysis_", tooday) 
rmarkdown::render(
  "water_chemistry_report.Rmd",
  output_format = 'pdf_document',
  output_dir    = results_dir,
  output_file = outname
)


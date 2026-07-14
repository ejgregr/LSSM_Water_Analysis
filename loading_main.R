# Main setup and control script for discrete data loading
# Script:  loading_man.R - Broughton LSSM version
# Created: February 2024. EJG
# Revisited June 2026. EJG
#   Initially included only earlier BATI data. Since then has been updated to include the 
#   2025 field field data from the Broughton work, as well as 2025 current data 
#   and PAR (solar) data sets for driving the ODE kelp model. 
#-------------------------------------------------------------------------------
# This script sources the necessary libraries and functions, coordinates the
# data loading, and creates some data structures. The data structures are exported
# to an RData file and loaded by the main model project.
#
# Documentation: The RMD file is the repository for data notes. Eventually, would 
# be nice if it created a data summary. 
#
## Updates:
# 2026/01/13: Loads discrete and mooring data, as well as necessary 2025 conditions
#   for growth model including tidal currents and light levels. 
# 2026/06/23: After a hiatus, backfilling and documenting before moving forward.
#  Checking out temperatures, as the DST temperature last saved seems to have a 
# step-up around day 50. 
#========================== Load required packages ============================

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

# Deployment dates for trimming sensor data
sdate <- "2025-04-25" # The day after deployment
edate <- "2025-09-15" # The day before mooring recovery




outname <- paste0("Broughton_Water_Analysis_", tooday) 
rmarkdown::render(
  "water_chemistry_report.Rmd",
  output_format = 'pdf_document',
  output_dir    = results_dir,
  output_file = outname
)


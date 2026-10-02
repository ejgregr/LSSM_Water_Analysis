#===============================================================================
# Script:  loading_main.R - Broughton LSSM version
# Purpose: Main setup and control script for Broughton data loading
# Created: February 2024. EJG
# Purpose: Source necessary libraries and functions, coordinate data loading and 
# creation of data structures, and export to an RData file for loading by the main project script.
# Documentation: The RMD file is the repository for data notes. Eventually, would 
# be nice if it created a data summary. 
#-------------------------------------------------------------------------------
# Updates:
# 2026/01/13: Loads discrete and mooring data, as well as necessary 2025 conditions
#   for growth model including tidal currents and light levels.
# June 2026.
#   Initially included only earlier BATI data and CO2 from MV Columbia. 
#   Updated to include the field data from the 2025 Broughton work, 
#   as well as 2025 tide height  and PAR (solar) data sets for driving the ODE kelp model. 
# 2026/06/23: After a hiatus, backfilling and documenting before moving forward.
#  Repaired DST temperature data 
# 2026/09/08: Code review taken up again after summer.

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


#=== LOAD Previously Saved Observational data ===
# LOAD 2025 observational data, including PAR/DLI estimates.
# LAST time RData created = 2026/01/13 by the LSSM_Water_analysis.proj
input_path <- file.path(results_dir, "kelp_project_data_2026_01_13.RData" )
#input_path <- file.path(DEB_dir, "kelp_project_data_2026_01_13.RData" )
#load(input_path)


#---- Load, prep, and save individual project data sets. 

# Star-Oddi sensors. 2 focal and 2 ref.
source( 'C:/Data/Git/LSSM_Water_Analysis/DST_loading.R')
# DFs: DST_foc1, DST_foc2, and DST_ref1, DST_ref2
str(DST_foc1)

# Show temperature timeseries
x <- list(
  list(df = DST_ref1,  var = "Temp", label = "DST Reference 1"),
  list(df = DST_ref2,  var = "Temp", label = "DST Reference 2"),
  list(df = DST_foc1,  var = "Temp", label = "DST Focal 1"),
  list(df = DST_foc2,  var = "Temp", label = "DST Focal 2") )
plot_timeseries( x, metric = "Temperature (°C)" )

# Show salinity timeseries
x <- list(
  list(df = DST_ref1,  var = "Salinity", label = "DST Reference 1"),
  list(df = DST_ref2,  var = "Salinity", label = "DST Reference 2"),
  list(df = DST_foc1,  var = "Salinity", label = "DST Focal 1"),
  list(df = DST_foc2,  var = "Salinity", label = "DST Focal 2") )
plot_timeseries( x, metric = "Salinity (psu)")

# MinoDOT sensor for DO and temp. 1 on each mooring
source( 'C:/Data/Git/LSSM_Water_Analysis/minidot_loading.R')
# DFs: mdot_foc and mdot_ref
# Vars: Temp, DO, DO_sat, Q

# Show DO timeseries
x <- list(
  list(df = mdot_foc,  var = "DO", label = "mDOT Focal"),
  list(df = mdot_ref,  var = "DO", label = "mDOT Reference") )
plot_timeseries( x, metric = "Dissovlved O2 (mg/L)")

# Show temperature timeseries
x <- list(
  list(df = mdot_foc,  var = "Temp", label = "mDOT Focal"),
  list(df = mdot_ref,  var = "Temp", label = "mDOT Reference") )
plot_timeseries( x, metric = "Temperature (°C)")

# CO2Pro sensor. Just the pCO2 thank you. 
# NOTE: Sensors take a triplicate every hour. 
source( 'C:/Data/Git/LSSM_Water_Analysis/CO2_data_loading.R')
# DFs: CO2_foc, CO2_ref
# Vars: pCO2 
x <- list(
  list(df = CO2_foc,  var = "CO2", label = "CO2Pro Focal"),
  list(df = CO2_ref,  var = "CO2", label = "CO2Pro Reference") )
plot_timeseries( x, metric = "partial CO2 (uatm)")


source( 'C:/Data/Git/LSSM_Water_Analysis/PAR_loading.R')
# DF: DLI
# DFs include: 
str( DLI_df )
DLI_df <- trim_deployment( DLI_df )
DLI_plot( DLI_df)

source( 'C:/Data/Git/LSSM_Water_Analysis/discrete_sample_loading.R')
# DF: clean_data
head( clean_data )


# PAR loading includes GEE export of ERA5 radiation. PAR and DLI derived.  
#   DLI_df

# Currents includes 5 min predictions of tide and direction and Weyton and Blackney passes
#   
# Output results from data loading ... 

df_names <- c( "CO2_focal", "CO2_ref", 
               "DST_focal1", "DST_focal2", "DST_ref1", "DST_ref2", 
               "mdot_focal", "mdot_ref", 
               "DLI_df", "par_df" )

# save(list = df_names, file = file.path(results_dir, "kelp_project_data.RData"))

# To load in a new script:
# load("kelp_project_data.RData")


#head(DST_focal1)
#head(mdot_ref_dat)


# Write documentation PDF from Markdown

outname <- paste0("Broughton_Water_Analysis_", tooday) 
rmarkdown::render(
  "water_chemistry_report.Rmd",
  output_format = 'pdf_document',
  output_dir    = results_dir,
  output_file = outname
)


#===============================================================================
# Script:  CO2_data_loading.R
# Purpose: Load CO2 Pro data from sensor files provided by Wiley. 
# Created: Oct 30, 2025
# NOTE: Significant bits of this code, particularly string processing, were provided by ChatGPT. 
#===============================================================================
# Updates:
# 2026/09/22: Reviewed and simplified as part of data loading code cleanup. 
#===============================================================================

### OUTPUTS are CO2_focal and CO2_ref.

#------ Data loading section ----
# Closer examination of matlab files shows Oct21 matlab files contain the full timeseries.

# get the individual files ... 
# extend file names with path  ... 
focal_file <- file.path( paste0( source_dir, "/CO2Pro/kelp_sensor_oct21_2025_43_213_75_matlab.txt") )
ref_file   <- file.path( paste0( source_dir, "/CO2Pro/ref_sensor_oct21_2025_43_214_75_matlab.txt") )

# Show the header row in each of the file sets.
#for (f in ref_files_full) {
#  cat("\n", f, ":\n")           # print the filename
#  cat(readLines(f, n = 1), "\n")  # print only the first line
#}

# define the columns to keep, after the time data is turned into Posix
# NB: Neither of the temps in the CO2Pro output are ambient.
cols_to_keep <- c("DateTime", "CO2")

# Data loading ... 
CO2_foc <- load_matlab( focal_file, cols_to_keep )
CO2_ref <- load_matlab( ref_file, cols_to_keep )

# Check length of the two time series ... 
difftime( CO2_foc[1,]$DateTime, CO2_foc[ dim(CO2_foc)[[1]], ]$DateTime )
difftime( CO2_ref[1,]$DateTime, CO2_ref[ dim(CO2_ref)[[1]], ]$DateTime )

#---- Tidy up the data ----
# Reference sensor valve (or something) was found too tight on July visit. 
# No data prior to that.
CO2_ref <- CO2_ref[ CO2_ref$DateTime > as.POSIXct("2025-07-11", tz = "America/Vancouver"), ]

# Removed manually identified maintenance periods
CO2_foc <- trim_foc_maintenance( CO2_foc )
CO2_ref <- trim_ref_maintenance( CO2_ref )

#---- Data visualization - Show clean CO2 data range from both moorings ---
# add columns with the dataset names so we have a legend
# 2026/09/22: Depreciated. Prefer to plot on separate panels. 
#CO2_foc$Dataset <- "Focal"
#CO2_ref$Dataset <- "Reference"
#plot_dat <- rbind(CO2_foc, CO2_ref)
# Plot of cleaned up data ON SAME GRAPH
#two_series_plot( plot_dat, "CO2", "pCO2 (μatm)", "Cleaned pCO2 Timeseries from Focal and Reference Moorings" )

### Fin.
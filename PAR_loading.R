#===============================================================================
# Script:  PAR_loading.R
# Purpose: Loads and processes hourly downward radiation from ERA5 sensor and 
#   converts it to DLI.
# Created: Dec, 2025
# NOTES: 
# CSV file contains hourly estimates of downward surface solar radiation. 
# Source data (ERA5) has large pixels, we use GEE to average 3 in the AOI. 
#===============================================================================
# Updates:
# 2026/07: Reviewed, simplified, and documented GEE methods for ERA5 extraction
#   See tech doc for details. 
#===============================================================================
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

# Load the ERA5 data ... NOTE: n11 has same structure so can be loaded instead
era5_df <- read.csv( paste0( source_dir, "/GEE_exports/ERA5_solar_pixels_n3.csv"))
head(era5_df)
names(era5_df)
nrow(era5_df)

# The ERA5 timestamp is in UTC, so the time needs to be adjusted. 

# Step 2: Shift the time to PST, this is -8 hrs (7 during DST in BC but ... )
era5_df$Timestamp <- as.POSIXct(era5_df$Timestamp, format = "%Y-%m-%d %H:%M", tz = "UTC")
era5_df$Timestamp <- format(era5_df$Timestamp, tz = "America/Vancouver", usetz = TRUE)

# Trim and rename ... 
era5_df <- era5_df[, c(-1, -3, -6)] # drop idx, count, and geo
names( era5_df ) <- c( "DateTime", "SSRD_mean", "SSRD_stdev") 

head(era5_df)
tail(era5_df)

DLI_df <- calc_dli( era5_df )
head(DLI_df)
DLI_plot( DLI_df )


#----
# FIN.


##### Radiation loading - from DSR to PAR #####
# Created Dec, 2025
#
# NOTES:
# CSVs (one per month) created using Python script in GEE. CSV files contain
# hourly estimates of downward surface solar radiation. NB the source data (ERA5)
# has large pixels.
# 
# See project report for details.
# 
############################################################################
library(ggplot2)
library(dplyr)
library(lubridate)
library(stringr)

#------------------------------------
# Function to adjust the time in the ERA5 data
shift_par_timestamps <- function(PAR_df, shift_hours, tz_in = "UTC", tz_out = "UTC") {
  
  # tolerant parse: handles "2025-04-01 00:00:00" and "2025-04-01 00:00:00 UTC"
  dt <- suppressWarnings(parse_date_time(
    as.character(PAR_df$Timestamp),
    orders = c("Y-m-d H:M:S", "Y-m-d H:M:S z", "Ymd HMS", "Ymd HMS z"),
    tz = tz_in
  ))
  
  n_bad <- sum(is.na(dt))
  if (n_bad > 0) {
    bad_examples <- unique(as.character(PAR_df$Timestamp)[is.na(dt)])
    bad_examples <- head(bad_examples, 5)
    stop(sprintf(
      "Timestamp parse failed for %d rows. Examples: %s",
      n_bad, paste(bad_examples, collapse = " | ")
    ))
  }
  
  dt_shifted <- dt + hours(shift_hours)
  
  PAR_df %>%
    mutate(
      DateTime_shifted = dt_shifted,
      Timestamp = format(with_tz(DateTime_shifted, tz_out), "%Y-%m-%d %H:%M:%S")
    ) %>%
    select(-DateTime_shifted)
}
#------------------------------------

#rm( list=c('era5_df','ERA_fixed','era_fixed'))

# Directory containing monthly CSVs
csv_dir <- "GEE_exports"

# Read and combine
files <- list.files( path = paste0(source_dir, "/GEE_exports"),
                             pattern = "\\.csv$",
                             full.names = TRUE )
era5_df <- do.call(
  rbind,
  lapply(files, function(f) {
    d <- read.csv(f, stringsAsFactors = FALSE)
    d$Timestamp <- as.POSIXct(d$Timestamp, tz = "UTC")
    d
  })
)

# The ERA5 code has a couple issues. First, it's UTC, so the time needs to be adjusted. 
# But before that, there is an odd thing where every 24th row has a malformed DateTime.

# Step 1: Find and fix malformed dates ... 
dt_try <- suppressWarnings(lubridate::ymd_hms(as.character(era5_df$Timestamp), tz="UTC"))
bad <- which(is.na(dt_try))

# Now replace bad rows by writing "YYYY-mm-dd 17:00:00"
# first coerce to character as its behaving like a factor
era5_df$Timestamp <- as.character(era5_df$Timestamp)

# now pull bad dates, strip trailing " UTC" if present
bad_dates <- sub(" UTC$", "", as.character( era5_df$Timestamp[ bad ]) )

# write the expanded, correct date-time format
era5_df$Timestamp[bad] <- paste0(bad_dates, " 00:00:00")

# Step 2: Shift the time to PST, this is -7 during daylight savings in BC
ERA_fixed <- shift_par_timestamps(era5_df, shift_hours = -7)


#---- CONVERT DSR to PAR ----
era5_df <- ERA_fixed

# Keep only required columns
# stdDev is across the 11 pixels selected by the Broughton mask
era5_df <- era5_df[, c("Timestamp", "mean", "stdDev")]

# Ensure time ordering
era5_df <- era5_df[order(era5_df$Timestamp), ]
head(era5_df)

# Convert DSR to PAR ... 
par_df <- era5_df
par_df$mean   <- (era5_df$mean   / 3600) * 0.5 * 4.57
par_df$stdDev <- (era5_df$stdDev / 3600) * 0.5 * 4.57

# Hourly plot is messy and time-consuming to produce. Plot daily if you like
#daily_PAR_plot( par_df )

# Sum PAR to get a daily light interval .... 

DLI_df <- calc_dli( par_df )
head(DLI_df)

DLI_plot( DLI_df )



#----
# FIN.


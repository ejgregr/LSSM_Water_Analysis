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
daily_PAR_plot( par_df )

# Sum PAR to get a daily light interval .... 

DLI_df <- calc_dli( par_df )
head(DLI_df)

DLI_plot( DLI_df )






#------------------------- PAR support FUNCTIONS -----------------------------

#---- Daily light interval from PAR ----
calc_dli <- function(df) {
  df$Date <- as.Date(df$Timestamp)
  
  # Sum the means per day and convert to mol/m2/d
  # (Sum * 3600 / 1000000 = Sum * 0.0036)
  daily_mean <- aggregate(mean ~ Date, data = df, FUN = function(x) sum(x) * 0.0036)
  colnames(daily_mean)[2] <- "mean"
  
  # Propagate uncertainty: sqrt(sum(stdDev^2)) * 0.0036
  daily_sd <- aggregate(stdDev ~ Date, data = df, FUN = function(x) sqrt(sum(x^2)) * 0.0036)
  colnames(daily_sd)[2] <- "dli_stdDev"
  
  # Merge results into a single data frame
  result <- merge(daily_mean, daily_sd, by = "Date")
  
  return(result)
}

#----- Plot DLI   ----
DLI_plot <-function( dli_dat ){
  
  ggplot(dli_dat, aes(x = Date, y = mean)) +
    geom_errorbar(
      aes(
        ymin = mean - stdDev,
        ymax = mean + stdDev
      ),
      width = 0,            # vertical lines only
      alpha = 0.5,
      linewidth = 0.4
    ) +
    geom_point(
      size = 1.2,
      color = "black"
    ) +
    labs(
      x = "Date",
      y = expression("Daily Light Interval (mol m"^-2~" d"^-1~")"),
      title = "Daily Light Interval (mean ± SD)"
    ) +
    theme_bw()
}

#----- Plot daily PAR data  ----
daily_PAR_plot <-function( par_dat ){
  
  # downsample to daily ... 
  par_daily <- aggregate(
    cbind(mean, stdDev) ~ as.Date(Timestamp),
    data = par_dat,
    FUN = mean
  )
  
  names(par_daily)[1] <- "Date"
  
  ggplot(par_daily, aes(x = Date, y = mean)) +
    geom_errorbar(
      aes(
        ymin = mean - stdDev,
        ymax = mean + stdDev
      ),
      width = 0,            # vertical lines only
      alpha = 0.5,
      linewidth = 0.4
    ) +
    geom_point(
      size = 1.2,
      color = "black"
    ) +
    labs(
      x = "Date",
      y = expression("Photosynthetically Active Radiation (umol m"^-2~" s"^-1~")"),
      title = "Daily Photosynthetically Active Radiation (mean ± SD)"
    ) +
    theme_bw()
}

#----
# FIN.
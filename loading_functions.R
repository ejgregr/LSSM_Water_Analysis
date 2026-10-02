#===============================================================================
# Script:  loading_functions.R
# Purpose: All functions to support loading and plotting
# Created: Nov 2025
# NOTES: Significant bits of this code, particularly string processing, were provided by ChatGPT.
#   Sensor data include depth, temperature, salinity, and conductivity
#===============================================================================
# Updates:
# July 2026: Revising as part of data review. So far have:
#   - new function to remove identified maintenance windows
#   - reviewed and revised MDOT data loading
#
# TO DO:
#         ** FIX THE DATA LOAD AND DATA SAVING CODE BELOW **
# Jan28: in progress. CO2 data combed thru. May be a merge issue with the data
# Next:
#   - isolate the outputs from each of the other data loading sheets.
#   - tidy up current data, esp. some methods.
#===============================================================================

#=============================== Common functions =============================
# Trim data set to deployment dates. Depends on global sdate and edate
trim_deployment <- function(df) {
  
  # The Deployment window first ... 
  df <- df[(
    df$DateTime >= as.POSIXct(sdate, tz = "America/Vancouver") &
    df$DateTime <= as.POSIXct(edate, tz = "America/Vancouver")
  ), ]

  df
}

# Trim mooring data with JUNE and JLUY maintenance windows identified by manual examination
# of the temperature data - NB: Windows DIFFER for each mooring.
trim_foc_maintenance <- function(df) {
  
  # Times from DST focal data:
#  jun12_start <- "2025-06-12 12:30:00"
#  jun12_end   <- "2025-06-12 13:30:00"
  # expanded to include DST Salinity anomaly. Bubble in sensor?
  jun12_start <- "2025-06-12 12:00:00"
  jun12_end   <- "2025-06-12 14:00:00"
  
  
  jul10_start <- "2025-07-10 13:00:00"
  jul10_end   <- "2025-07-10 14:15:00"

  aug25_start <- "2025-08-25 10:15:00"
  aug25_end   <- "2025-08-25 13:00:00"
  
  windows <- list(
    c(jun12_start, jun12_end),
    c(jul10_start, jul10_end),
    c(aug25_start, aug25_end)
 )
  
  for (w in windows) {
    df <- df[!(df$DateTime >= as.POSIXct(w[1], tz = "America/Vancouver") &
               df$DateTime <= as.POSIXct(w[2], tz = "America/Vancouver")), ]
  }
  df
}

trim_ref_maintenance <- function(df) {
  
  jun12_start <- "2025-06-12 10:00:00"  # This captures a salinity anomaly earlier in the day
  #jun12_start <- "2025-06-12 14:00:00"
  jun12_end   <- "2025-06-12 16:30:00"

  jul10_start <- "2025-07-10 10:00:00" 
  jul10_end   <- "2025-07-10 13:00:00"
  
  aug25_start <- "2025-08-25 12:30:00"
  aug25_end   <- "2025-08-25 14:00:00"

  windows <- list(
    c(jun12_start, jun12_end),
    c(jul10_start, jul10_end),
    c(aug25_start, aug25_end)
  )
  
  for (w in windows) {
    df <- df[!(df$DateTime >= as.POSIXct(w[1], tz = "America/Vancouver") &
               df$DateTime <= as.POSIXct(w[2], tz = "America/Vancouver")), ]
  }
  df
}

# Insert an NA row in the maintenance gaps so the graphs do not interpolate
# FOR PLOTTING ONLY
insert_gap <- function(df, time) {
  gap_row <- df[1, ]
  gap_row[, !names(gap_row) %in% "DateTime"] <- NA
  gap_row$DateTime <- as.POSIXct(time, tz = "America/Vancouver")
  df <- rbind(df, gap_row)
  df[order(df$DateTime), ]
}

# Plot a set of time series on their own axes
plot_timeseries <- function(series_list,
                            metric = "Value",
                            colours  = c("steelblue", "darkred", "darkgreen", "purple", "darkorange")) {
  n <- length(series_list)
  
  par(mfrow = c(n, 1), mar = c(2, 4, 1, 1))
  
  for (i in seq_along(series_list)) {
    s <- series_list[[i]]
    plot(
      s$df$DateTime,
      s$df[[s$var]],
      type = "l",
      col = colours[i],
      lwd = 0.8,
      xlab = "",
      ylab = metric,
      main = s$label
    )
  }
  
  mtext("Date", side = 1, line = 3)
  par(mfrow = c(1, 1))
}


#==================================== CO2 Data =================================
# Simplified load for CO2Pro single matlab scripts 
load_matlab <- function(datfile, cols, tz = "America/Vancouver") {
  df <- read.delim(
    datfile,
    header = TRUE,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  names(df) <- sub("^%", "", names(df)) # clean up Year name
  df <- df[-1, , drop = FALSE]                    # drop the formatting row
  df[] <- lapply(df, type.convert, as.is = TRUE)  # restore numeric types
  df$DateTime <- as.POSIXct(
    sprintf(
      "%04d-%02d-%02d %02d:%02d:%02d",
      df$Year,
      df$Month,
      df$Day,
      df$Hour,
      df$Minute,
      df$Second
    ),
    tz = tz
  )
  df <- df[, cols, drop = FALSE] # keep only needed columns
  rownames(df) <- NULL
  df
}


# 2026/09/22: Depreciated. Prefer to plot on separate panels. 
# Plot two comparable time series.
# Column to plot, y axis title, and main title are parameters
# Required values include "DateTime" and "Dataset"

two_series_plot <- function(plot_dat, y_val, y_text, t_text, sdate = NULL, edate = NULL) {
  
  if (!is.null(sdate) & !is.null(edate)) {
    plot_dat <- plot_dat[plot_dat$DateTime >= as.POSIXct(sdate, tz = "America/Vancouver") &
                         plot_dat$DateTime <= as.POSIXct(edate, tz = "America/Vancouver"), ]
  }
  
  ggplot(plot_dat, aes(x = DateTime, y = .data[[y_val]], color = Dataset)) +
    geom_line(alpha = 0.8) +
    labs(x = "Time", y = y_text, color = "Dataset", title = t_text) +
    theme_bw()
}


# 2026/09/22: Depreciated. Prefer to plot on separate panels. 
plot_diff <-function( plot_dat ){
  ggplot(plot_dat, aes(x = Timestamp, y = CO2_diff)) +
    geom_line(color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Time",
      y = "CO₂ Difference (Focal − Reference)",
      title = "CO₂ Difference Over Time"
    ) +
    theme_bw()
}

# Reduces temporal resolution to hourly, and computes mean and sd by hour
# 2026/09/23: NOTE: SD likely meaningless as CO2 collects triplicates every hour.
hourly_stats <- function(df) {
  # Ensure Timestamp is POSIXct
  df$Timestamp <- as.POSIXct(df$Timestamp)
  
  # Create an hourly timestamp (truncate to hour)
  df$Hour <- as.POSIXct(format(df$Timestamp, "%Y-%m-%d %H:00:00"), tz = attr(df$Timestamp, "tzone"))
  
  # Compute mean and sd by hour
  agg_mean <- aggregate(CO2 ~ Hour, df, mean)
  agg_sd   <- aggregate(CO2 ~ Hour, df, sd)
  
  # Merge results
  result <- merge(agg_mean, agg_sd, by = "Hour", suffixes = c("_mean", "_sd"))
  
  result
}

# As above but daily.
daily_stats <- function(df) {
  # Ensure Timestamp is POSIXct (no harm if it already is)
  df$Timestamp <- as.POSIXct(df$Timestamp)
  
  # Make a daily date column
  df$Date <- as.Date(df$Timestamp)
  
  # Mean per day
  mean_daily <- aggregate(CO2 ~ Date, df, mean)
  
  # SD per day (use 0 if only one sample that day)
  sd_daily <- aggregate(CO2 ~ Date, df, function(x) if (length(x) > 1) sd(x) else 0)
  
  # Merge results
  result <- merge(mean_daily, sd_daily, by = "Date", suffixes = c("_mean", "_sd"))
  
  return(result)
}


#========================== MiniDot (O2 and T) Data ============================
# Load and combine concatenated files.
# 09/25: UPDATED read_mdot to process Unix date for consistency. Fuck. :\
read_mdot <- function(files, cols = c("DateTime", "Temp", "DO")) {
  do.call(rbind, lapply(files, function(f) {
    df <- read.csv(f, skip = 9, header = FALSE)
    names(df) <- c("Unix_date", "DateTime", "DateTime_UTC",
                   "Battery", "Temp", "DO", "DO_sat", "Q")
#    df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "America/Vancouver")
    df$DateTime <- as.POSIXct(df$Unix_date, origin = "1970-01-01", tz = "America/Vancouver")
    df[, cols]
  }))
}

# Load and combine daily txt files to fill the mDOT time gap.
read_txts <- function(files, cols = c("DateTime", "Temp", "DO")) {
  do.call(rbind, lapply(files, function(f) {
    df <- read.csv(f, skip = 3, header = FALSE)
    names(df) <- c("Unix_date", "Battery", "Temp", "DO", "Q")
    df$DateTime <- as.POSIXct(df$Unix_date, origin = "1970-01-01", tz = "America/Vancouver")
    df[, cols]
  }))
}


#------------------------- PAR support FUNCTIONS -----------------------------
#---- Daily light interval from PAR ----
calc_dli <- function(df) {
  
  # Convert SSRD (J/m2/hr) to PAR (mol photons m-2 hr-1)
  # 0.45 = PAR energy portion of total solar radiation
  # 4.57 = umol of photons in PAR 
  # 1e6  = umol to mol 
  df$PAR_mol_hr    <- df$SSRD_mean   * 0.45 * 4.57 / 1e6
  df$PAR_var_hr    <- (df$SSRD_stdev * 0.45 * 4.57 / 1e6)^2  # variance
  
  # Date column for aggregation
  df$Date <- as.Date(df$DateTime)
  
  # Daily DLI — sum hourly PAR and variance, then take sqrt for SD
  DLI_mean <- aggregate(PAR_mol_hr ~ Date, data = df, FUN = sum)
  DLI_var  <- aggregate(PAR_var_hr ~ Date, data = df, FUN = sum)
  
  data.frame(
    DateTime = as.POSIXct(paste(DLI_mean$Date, "12:00:00"),
                          format = "%Y-%m-%d %H:%M:%S",
                          tz     = "America/Vancouver"),
    DLI_mean  = DLI_mean$PAR_mol_hr,
    DLI_sd    = sqrt(DLI_var$PAR_var_hr)
  )
}

# older version 
# calc_dli <- function(df) {
#   df$Date <- as.Date(df$Timestamp)
#   
#   # Sum the means per day and convert to mol/m2/d
#   # (Sum * 3600 / 1000000 = Sum * 0.0036)
#   daily_mean <- aggregate(mean ~ Date, data = df, FUN = function(x) sum(x) * 0.0036)
#   colnames(daily_mean)[2] <- "DLI_mean"
#   
#   # Propagate uncertainty: sqrt(sum(stdDev^2)) * 0.0036
#   daily_sd <- aggregate(stdDev ~ Date, data = df, FUN = function(x) sqrt(sum(x^2)) * 0.0036)
#   colnames(daily_sd)[2] <- "DLI_stdDev"
#   
#   # Merge results into a single data frame
#   result <- merge(daily_mean, daily_sd, by = "Date")
#   
#   return(result)
# }


#----- Plot DLI   ----
DLI_plot <-function( dli_dat ){
  
  ggplot(dli_dat, aes(x = DateTime, y = DLI_mean)) +
    geom_errorbar(
      aes(
        ymin = DLI_mean - DLI_sd,
        ymax = DLI_mean + DLI_sd
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
      title = "Daily Light Interval across 5 ERA5 pixels (mean ± SD)"
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
### FIN.
# All functions to support loading and plotting
# Created: December 2025. EJG
################################################################################
# UPDATES:
#
#
#
#==================================== CO2 Data =================================

# This function loads and combines the 4 CO2 files from each mooring to create 
# the full time series. There is one annoyance in that the reference site data
# has one file that is tab delimited, while the other 7 files are comma delimited. 
# This requires the condiional, delimiter-based read in the function.  
load_and_bind <- function(files, cols, tz = "UTC") {
  out <- do.call(rbind, lapply(files, function(f) {
    
    # --- Detect delimiter from first line ---
    first_line <- readLines(f, n = 1)
    is_comma <- grepl(",", first_line)
    
    # --- Read file accordingly ---
    if (is_comma) {
      df <- read.csv(f, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      df <- read.delim(f, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    df <- df[-1, , drop = FALSE]                    # drop the formatting row
    df[] <- lapply(df, type.convert, as.is = TRUE)  # restore numeric types
    df <- df[, cols, drop = FALSE]                  # keep only needed columns
    df$Timestamp <- as.POSIXct(sprintf(
      "%04d-%02d-%02d %02d:%02d:%02d",
      df$Year, df$Month, df$Day, df$Hour, df$Minute, df$Second
    ), tz = tz)
    df
  }))
  rownames(out) <- NULL
  out
}

# Plot two comparable time series. 
# Column to plot, y axis title, and main title are parameters
# Required values include "Timestamp" and "Dataset"
two_series_plot <-function( plot_dat, y_val, y_text, t_text ){
  ggplot(plot_dat, aes(x = Timestamp, y = .data[[y_val]], color = Dataset)) +
    geom_line(alpha = 0.8) +
    labs(
      x = "Time",
      y = y_text,
      color = "Dataset",
      title = t_text
    ) +
    theme_bw()
}  








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

# Simple function to remove dates 
trim_dates <- function(df) {
  
  df$Date <- as.Date(df$Timestamp)
  
  df <- df[df$Date >= start_date & df$Date <= end_date, ]
  df <- df[! df$Date %in% service_days, ]
  
  df$Date <- NULL   # optional cleanup
  df
}

plot_range <- function(df, start_date, end_date) {
  
  # Convert date inputs to Date objects (safe even if passed as Date already)
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  # Filter by date range
  df$Date <- as.Date(df$Timestamp)
  df_sub  <- df[df$Date >= start_date & df$Date <= end_date, ]
  
  # Plot
  library(ggplot2)
  ggplot(df_sub, aes(x = Timestamp)) +
    geom_line(aes(y = Temp_ref,   color = "Reference"), linewidth = 0.8) +
    geom_line(aes(y = Temp_focal, color = "Focal"),     linewidth = 0.8) +
    labs(
      x = "Time",
      y = "Temperature (°C)",
      color = "Sensor",
      title = paste0("Temperature Comparison: ", start_date, " to ", end_date)
    ) +
    scale_color_manual(values = c("Reference" = "blue", "Focal" = "red")) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

#=========================== StarODDI (S/T) Data ===============================

# Trim rows between one or more MATLAB-datenum intervals (inclusive)
# df must have a POSIXct column named DateTime (ChatGPT)
trim_by_matlab_windows <- function(df, intervals, tz = "UTC") {
  stopifnot("DateTime" %in% names(df))
  # ensure POSIXct and consistent tz
  df$DateTime <- as.POSIXct(df$DateTime, tz = tz)
  
  # helper: MATLAB datenum -> POSIXct
  m2t <- function(x) as.POSIXct((x - 719529) * 86400,
                                origin = "1970-01-01", tz = tz)
  
  # data bounds
  tmin <- min(df$DateTime, na.rm = TRUE)
  tmax <- max(df$DateTime, na.rm = TRUE)
  
  # expect a data.frame with columns 'start' and 'end' (numeric MATLAB datenums, 0 allowed)
  keep <- rep(TRUE, nrow(df))
  for (i in seq_len(nrow(intervals))) {
    s_raw <- intervals$start[i]
    e_raw <- intervals$end[i]
    s <- if (s_raw == 0) tmin else m2t(s_raw)
    e <- if (e_raw == 0) tmax else m2t(e_raw)
    keep <- keep & !(df$DateTime >= s & df$DateTime <= e)
  }
  df[keep, , drop = FALSE]
}

# This displays the intervals defined using MatLab time intervals to readable Posix
intervals_to_posix <- function(df, intervals, tz = "UTC") {
  stopifnot("DateTime" %in% names(df))
  
  # Ensure DateTime is POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, tz = tz)
  
  # Helper to convert MATLAB datenum to POSIXct
  m2t <- function(x) as.POSIXct((x - 719529) * 86400,
                                origin = "1970-01-01", tz = tz)
  
  # Bounds of actual data
  tmin <- min(df$DateTime, na.rm = TRUE)
  tmax <- max(df$DateTime, na.rm = TRUE)
  
  # Convert the intervals
  out <- intervals
  out$start_posix <- ifelse(out$start == 0, tmin, m2t(out$start))
  out$end_posix   <- ifelse(out$end   == 0, tmax, m2t(out$end))
  
  out
}


### FIN.
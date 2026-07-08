# All functions to support loading and plotting
# Created: December 2025. EJG
################################################################################
# UPDATES:
# none yet.
#
# TO DO:
#         ** FIX THE DATA LOAD AND DATA SAVING CODE BELOW **
# Jan28: in progress. CO2 data combed thru. May be a merge issue with the data
# Next: 
#   - isolate the outputs from each of the other data loading sheets. 
#   - tidy up current data, esp. some methods.
################################################################################

# DST data includes Temp, Salinity, Conductivity, DateTime
#   DST_focal1, DST_focal2, DST_ref1, DST_ref2

# Minidot includes Temp, DO, DO_sat, Q
#   mdot_focal, mdot_ref

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

# Load and combine catenated files.
read_mdot <- function(files, cols = c("DateTime", "Temp", "DO", "DO_sat")) {
  do.call(rbind, lapply(files, function(f) {
    df <- read.csv(f, skip = 9, header = FALSE)
    names(df) <- c("Unix_date", "DateTime", "DateTime_UTC",
                   "Battery", "Temp", "DO", "DO_sat", "Q")
    df$DateTime <- as.POSIXct(df$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    df[, cols]
  }))
}

# Load and combine daily txt files to fill the mDOT time gap.
read_txts <- function(files, cols = c("DateTime", "Temp", "DO")) {
  do.call(rbind, lapply(files, function(f) {
    df <- read.csv(f, skip = 3, header = FALSE)
    names(df) <- c("Unix_date", "Battery", "Temp", "DO", "Q")
    df$DateTime <- as.POSIXct(df$Unix_date, origin = "1970-01-01", tz = "UTC")
    df[, cols]
  }))
}



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
# df must have a POSIXct column named DateTime
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


#------------------------- PAR support FUNCTIONS -----------------------------
#---- Daily light interval from PAR ----
calc_dli <- function(df) {
  df$Date <- as.Date(df$Timestamp)
  
  # Sum the means per day and convert to mol/m2/d
  # (Sum * 3600 / 1000000 = Sum * 0.0036)
  daily_mean <- aggregate(mean ~ Date, data = df, FUN = function(x) sum(x) * 0.0036)
  colnames(daily_mean)[2] <- "DLI_mean"
  
  # Propagate uncertainty: sqrt(sum(stdDev^2)) * 0.0036
  daily_sd <- aggregate(stdDev ~ Date, data = df, FUN = function(x) sqrt(sum(x^2)) * 0.0036)
  colnames(daily_sd)[2] <- "DLI_stdDev"
  
  # Merge results into a single data frame
  result <- merge(daily_mean, daily_sd, by = "Date")
  
  return(result)
}

#----- Plot DLI   ----
DLI_plot <-function( dli_dat ){
  
  ggplot(dli_dat, aes(x = Date, y = DLI_mean)) +
    geom_errorbar(
      aes(
        ymin = DLI_mean - DLI_stdDev,
        ymax = DLI_mean + DLI_stdDev
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


#--------------- FUNCTIONS - Tides and Currents -------------------------------
load_currents <- function(filename, tz = "UTC") {
  
  # Read all lines
  lines <- readLines(filename)
  
  # Identify first data line (starts with YYYY/MM/DD)
  data_start <- grep("^\\d{4}/\\d{2}/\\d{2}", lines)[1]
  if (is.na(data_start)) {
    stop("No data lines found in file: ", filename)
  }
  
  # Read only the data portion
  df <- read.table( text = lines[data_start:length(lines)],
                    header = FALSE,
                    col.names = c("Date", "HourMinute", "Direction", "Speed"),
                    stringsAsFactors = FALSE )
  
  # Build "YYYY/MM/DD HHMM" string
  time_str <- paste(df$Date, df$HourMinute) # need the space btwn date and time
  
  # Create POSIXct datetime
  df$Timestamp <- as.POSIXct(time_str, format = "%Y/%m/%d %H:%M", tz = tz)
  
  # Reorder columns
  df <- df[, c("Timestamp", "Date", "HourMinute", "Direction", "Speed")]
  
  return(df)
}


classify_ebb_flood <- function(df,
                               flood_range = c(315, 45),
                               ebb_range   = c(135, 225)) {
  
  dir <- df$Direction
  
  # Flood sector spans across 360 → handle wrap-around
  in_flood <- (dir >= flood_range[1] | dir <= flood_range[2])
  in_ebb   <- (dir >= ebb_range[1]   & dir <= ebb_range[2])
  
  state <- ifelse(in_flood, "flood",
                  ifelse(in_ebb, "ebb", "other"))
  
  df$FlowState <- state
  df
}

detect_transitions <- function(df) {
  state <- df$FlowState
  
  # Lagged state for comparison
  prev_state <- dplyr::lag(state)
  
  transitions <- which(state != prev_state & !is.na(prev_state))
  
  tibble::tibble(
    Timestamp  = df$Timestamp[transitions],
    From       = prev_state[transitions],
    To         = state[transitions]
  )
}

make_flow_windows <- function( transitions, series_start = NULL, series_end = NULL) {
  stopifnot(all(c("Timestamp", "From", "To") %in% names(transitions)))
  
  # Ensure sorted by time
  tr <- transitions[order(transitions$Timestamp), ]
  
  # 1) Collapse 'other' bridges into single ebb<->flood transitions
  collapsed <- list()
  i <- 1
  n <- nrow(tr)
  
  while (i <= n) {
    # Pattern: X -> other  then  other -> Y
    if (tr$To[i] == "other" && i < n && tr$From[i + 1] == "other") {
      from_state <- tr$From[i]
      to_state   <- tr$To[i + 1]      # should be 'ebb' or 'flood'
      t1 <- tr$Timestamp[i]
      t2 <- tr$Timestamp[i + 1]
      mid_time <- t1 + (t2 - t1) / 2  # midpoint
      
      collapsed[[length(collapsed) + 1]] <- data.frame(
        Timestamp = mid_time,
        From = from_state,
        To   = to_state,
        stringsAsFactors = FALSE
      )
      i <- i + 2  # skip the pair
    } else if (tr$From[i] != "other" && tr$To[i] != "other") {
      # Keep direct ebb<->flood transitions
      collapsed[[length(collapsed) + 1]] <- tr[i, ]
      i <- i + 1
    } else {
      # Transitions involving 'other' at start/end that we can't bridge cleanly
      i <- i + 1
    }
  }
  
  if (length(collapsed) == 0) {
    stop("No usable ebb/flood transitions after collapsing 'other' states.")
  }
  
  tr2 <- do.call(rbind, collapsed)
  tr2 <- tr2[order(tr2$Timestamp), ]
  
  # 2) Build ebb/flood windows
  
  # If user didn't give start/end, default to first/last transition times
  if (is.null(series_start)) series_start <- tr2$Timestamp[1]
  if (is.null(series_end))   series_end   <- tr2$Timestamp[nrow(tr2)]
  
  starts <- c()
  ends   <- c()
  flows  <- c()
  
  cur_state <- tr2$From[1]
  cur_start <- series_start
  
  for (k in seq_len(nrow(tr2))) {
    t_k <- tr2$Timestamp[k]
    
    # Close current window at this transition time
    starts <- c(starts, cur_start)
    ends   <- c(ends, t_k)
    flows  <- c(flows, cur_state)
    
    # New state starts at this time
    cur_state <- tr2$To[k]
    cur_start <- t_k
  }
  
  # Final window from last transition to series_end
  if (series_end > cur_start) {
    starts <- c(starts, cur_start)
    ends   <- c(ends, series_end)
    flows  <- c(flows, cur_state)
  }
  out <- data.frame(
    StartTime = as.POSIXct(starts, tz = "UTC"),
    EndTime   = as.POSIXct(ends, tz = "UTC"),
    Flow      = flows,
    stringsAsFactors = FALSE
  )
  out
}

speed_by_window <- function(windows, tide) {
  
  # Ensure time columns are POSIXct
  windows$StartTime <- as.POSIXct(windows$StartTime, tz = "UTC")
  windows$EndTime   <- as.POSIXct(windows$EndTime,   tz = "UTC")
  tide$Timestamp    <- as.POSIXct(tide$Timestamp,    tz = "UTC")
  
  # Prepare output columns
  avg_speed <- numeric(nrow(windows))
  sd_speed  <- numeric(nrow(windows))
  n_points  <- integer(nrow(windows))
  
  # Loop through each window
  for (i in seq_len(nrow(windows))) {
    
    start_i <- windows$StartTime[i]
    end_i   <- windows$EndTime[i]
    
    # Logical mask for data in this window
    sel <- tide$Timestamp >= start_i & tide$Timestamp < end_i
    
    speeds <- tide$Speed[sel]
    
    if (length(speeds) == 0) {
      avg_speed[i] <- NA
      sd_speed[i]  <- NA
      n_points[i]  <- 0
    } else {
      avg_speed[i] <- mean(speeds, na.rm = TRUE)
      sd_speed[i]  <- sd(speeds, na.rm = TRUE)
      n_points[i]  <- length(speeds)
    }
  }
  
  # Return a combined data frame
  out <- cbind(
    windows,
    avg_speed = avg_speed,
    sd_speed  = sd_speed,
    n_points  = n_points
  )
  
  return(out)
}



### FIN.
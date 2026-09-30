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

# Make time matrix from an existing sensor DF name, or from specified start/end dates. 
make_time_grid <- function(sensor_df, sdate = NULL, edate = NULL) {
  
  t_min <- min(sensor_df$DateTime, na.rm = TRUE)
  t_max <- max(sensor_df$DateTime, na.rm = TRUE)
  
  if (!is.null(sdate)) t_min <- as.POSIXct(sdate, tz = "America/Vancouver")
  if (!is.null(edate)) t_max <- as.POSIXct(edate, tz = "America/Vancouver")
  
  t_grid <- seq(from = t_min, to = t_max, by = "5 min")
  
  data.frame(
    DateTime = t_grid,
    mdot_foc = interp_temp(mdot_foc, t_grid),
    DST_foc1 = interp_temp(DST_foc1, t_grid),
    DST_foc2 = interp_temp(DST_foc2, t_grid)
  )
}

# Pass the longest sensor df to match the deviations time grid
temp_mat        <- make_time_grid(mdot_foc)
tides_mdot_5min <- predict_tides(tides, temp_mat)
tdevs           <- cbind(tides_mdot_5min, deviations)

interp_temp <- function(df, t_grid) {
  approx(df$DateTime, df$Temp, xout = t_grid, method = "linear")$y
}

# Predict tide heights from Alert Bay data to 5 minute time intervales from sensors.
predict_tides <- function(tides, target_df) {
  
  t0 <- min(tides$DateTime)
  tides$hours <- as.numeric(difftime(tides$DateTime, t0, units = "hours"))
  
  # Tidal constituent periods (hours)
  M2 <- 12.4206
  S2 <- 12.0000
  K1 <- 23.9345
  O1 <- 25.8194
  
  harm_fit <- lm(Metres ~ 
                   sin(2*pi*hours/M2) + cos(2*pi*hours/M2) +
                   sin(2*pi*hours/S2) + cos(2*pi*hours/S2) +
                   sin(2*pi*hours/K1) + cos(2*pi*hours/K1) +
                   sin(2*pi*hours/O1) + cos(2*pi*hours/O1),
                 data = tides)
  
  # Predict at target timestamps
  hours_out <- as.numeric(difftime(target_df$DateTime, t0, units = "hours"))
  pred_df   <- data.frame(hours = hours_out)
  
  data.frame(
    DateTime  = target_df$DateTime,
    tide_m    = predict(harm_fit, newdata = pred_df)
  )
}

# Functions for defining flow direction and windows
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


#----
### FIN.
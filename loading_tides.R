# #============================================================================
# Script:  loading_tides.R
# Created: Late 2025
# Updated: 2026/11/18
# Purpose: Code and support functions to Load, model, and scale tides to support analysis. 
# Load hourly tide heights from a CSV obtained from https://tides.gc.ca/en/stations/08325.
# The closest station to Village Sea is Cedar Island. A prediction station.
#Returns:
#   1) 2020-2028 hourly tide height predictions Cedar Island.  
#   2) the de-trended tide height vs temperature plot, 
#   3) the CCF plots of tide vs. temperature at both mooring sites.
#============================================================================

#---------------------------- Functions ------------------------------

plot_prop_lag <- function( temp_df, tides ){
  mdot_cc        <- temp_df
  mdot_cc$tide   <- tides$tide_m
  mdot_cc        <- mdot_cc[complete.cases(mdot_cc[, c("Temp", "tide")]), ]
  
  # Detrend both series to remove seasonal signal
  # THIS TAKES SOME TIME ... 
  mdot_cc$Temp_dt <- residuals(loess(Temp ~ as.numeric(DateTime), data = mdot_cc, span = 0.1))
  mdot_cc$tide_dt <- residuals(loess(tide ~ as.numeric(DateTime), data = mdot_cc, span = 0.1))
  
  
  #---- Cross correlation function: negative lag = tide leads temperature ----
  ccf(mdot_cc$tide_dt, mdot_cc$Temp_dt,
      lag.max = 360,
      main    = "CCF: Detrended Tide vs Temperature — Focal Site",
      ylab    = "CCF",
      xlab    = "Lag (observations at 1-min resolution)")
}

load_tidal_data <- function( tfile ){
# Re: Cedar Island data: Both the data file header and the metadata identify the tz 
# as "Canada/Pacific". However, tests with Claude show that the imported CSV does not
# correctly adjust for DST. i.e., These times would show the relevant adjustments:
#   raw[grepl("2025/03/10 0[1-3]", raw$DateTime), ]          # spring forward
#   raw[grepl("2025/11/03 0[0-2]", raw$DateTime), ]          # fall back
# --> Indicates file is in fixed PST, despite the metadata. :\
#     PST is coded as "Etc/GMT+8". That’s UTC−8 with no DST.
  
# Skip the header lines and assign column names.
  df <- read.csv( tfile, header = FALSE, stringsAsFactors = FALSE,
                  skip = 7, col.names = c("DateTime", "Metres") )
    # Parse datetime
  df$DateTime <- as.POSIXct(df$DateTime, format = "%Y/%m/%d %H:%M", tz = "Etc/GMT+8")
  df
}

# Predict tide heights at for specified time intervals (target_df).
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
    Metres    = predict(harm_fit, newdata = pred_df)
  )
}

load_current_data <- function(filename, tz = "America/Vancouver") {
# *NOT YET REVIEWED FOR TIME HANDLING*
  
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

# Create time matrix using either existing sensor DF name, or from start/end dates. 
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

# Support function for make_time_grid()
interp_temp <- function(df, t_grid) {
  approx(df$DateTime, df$Temp, xout = t_grid, method = "linear")$y
}


#---- group of functions to process current direction, speed, and tide windows
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


#---------------------------- Build some things  ------------------------------
# Settled on predictions from Cedar Island (some earlier work used Alert Bay). 
# Source file contains hourly predictions for Cedar Island for 2020 to 2025. 

#---- Load Cedar Island tide heights (2020 to 2028)
tides <- load_tidal_data( paste0(source_dir, 
                                 "/tidalflow/cedar_island_tides_2020to2028.csv") )


attr(tides$DateTime, "tzone") <- "America/Vancouver"   # display only


#--- Some plotting of what we've done.
# First, Nov Fall back period
p_tides <- subset(tides, DateTime >= as.POSIXct("2025-11-01", tz = "America/Vancouver") &
                         DateTime  < as.POSIXct("2025-11-04", tz = "America/Vancouver"))
#   raw[grepl("2025/03/10 0[1-3]", raw$DateTime), ]          # spring forward
#   raw[grepl("2025/11/03 0[0-2]", raw$DateTime), ]          # fall back

plot(p_tides$DateTime, p_tides$Metres, type = "l",main = "Cedar Island Water Level (November 2025)",
     xlab = "Date", ylab = "Water Level (m)")

# Just 1 week ... 
p_tides <- subset(tides, DateTime >= as.POSIXct("2025-07-01", tz = "America/Vancouver") &
                         DateTime  < as.POSIXct("2025-07-13", tz = "America/Vancouver"))

plot(p_tides$DateTime, p_tides$Metres, type = "l", main = "Cedar Island Water Level (July 2025)",
     xlab = "Date", ylab = "Water Level (m)")


# Trim the tidal data to the deployment period
tides <- subset(tides, DateTime >= as.POSIXct( sdate, tz = "America/Vancouver") &
                       DateTime  < as.POSIXct( edate, tz = "America/Vancouver"))


# fin.
# #============================================================================
# Script:  loading_tides.R
# Purpose: Code and support functions to Load, model, and scale tides to support analysis. 
# Load tidal data and examine role with respect to temperature and flow thru Canoe pass. 
# Includes:
#   1) 2025 annual tidal observations (predictions?) from Alert Bay.  
#   2) the de-trended tide height vs temperature plot, 
#   3) the CCF plots of tide vs. temperature at both mooring sites.
#============================================================================

#---------------------------- Functions ------------------------------

# Plot temperature variance vs. tidal range 
plot_tvar_vs_tides <- function( rolldevs, tides ) {
  #--- dampen sd and prep df ...
  daily_tdev_sd <- tapply(rolldevs, as.Date(tides$DateTime), mean, na.rm = TRUE)
  
  plot_tdev <- data.frame(Date = as.POSIXct(names(daily_tdev_sd)), sd   = as.numeric(daily_tdev_sd))
  
  #--- dampen tidal data  and prep df ...
  daily_tide_range <- tapply(tides$tide_m, as.Date(tides$DateTime), function(x)
    diff(range(x, na.rm = TRUE)))
  
  plot_tides <- data.frame(Date    = as.POSIXct(names(daily_tide_range)),
                           t_range = as.numeric(daily_tide_range))
  # drop last elements as tides have a 0 at the end
  plot_tdev  <- plot_tdev [-nrow(plot_tdev), ]
  plot_tides <- plot_tides[-nrow(plot_tides), ]
  tail(plot_tides)
  
  #--- Plot it up
  ylim_sd    <- range(plot_tdev$sd, na.rm = TRUE)
  ylim_range <- range(plot_tides$t_range, na.rm = TRUE)
  
  par(mar = c(5, 4, 4, 4) + 0.1)
  
  plot(
    plot_tides$Date,
    plot_tides$t_range,
    type = "l",
    col = "darkorange",
    lwd = 2,
    xlab = "Date",
    ylab = "Daily Tidal Range (m)",
    ylim = ylim_range,
    main = "Temperature variance vs. tidal range"
  )
  
  par(new = TRUE)
  plot(
    plot_tdev$Date,
    plot_tdev$sd,
    type = "l",
    col = "steelblue",
    lwd = 2,
    axes = FALSE,
    xlab = "",
    ylab = "",
    ylim = ylim_sd
  )
  
  axis(side = 4)
  mtext("Daily Mean Rolling SD (°C)", side = 4, line = 3)
  
  legend(
    "topleft",
    legend = c("Daily Tidal Range (m)", "Daily Mean Rolling SD (°C)"),
    col    = c("darkorange", "steelblue"),
    lty    = 1,
    lwd = 2,
    bty = "n"
  )#, text.font = 2)
}

#---------------------------- Do things  ------------------------------
#---- Load 2025 Alert Bay tide heights and calculate deviance ----
tide_file <- paste0(source_dir, "/tidalflow/Annual_Predictions_Alert Bay_2025.csv")
tides <- read.csv(tide_file, header = TRUE)
names(tides) <- c("DateTime", "Metres")
tides$DateTime <- as.POSIXct(tides$DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Vancouver")
tides$Metres   <- as.numeric(as.character(tides$Metres))

# Trim to mooring deployment period
tides <- tides[tides$DateTime >= as.POSIXct(sdate, tz = "America/Vancouver") &
               tides$DateTime <= as.POSIXct(edate, tz = "America/Vancouver"), ]

#---- Prepare timeseries for comparative analytics

# Use the existing grid from deviations df to make the predictions. See data_validation.R
tides_1min <- predict_tides(tides, data.frame(DateTime = deviations$t_grid))

#---- Pepare temperature deviations for plotting 
# tdevs = 'tides with temp deviations'. Time intervals need to match
tdevs <- data.frame(
  DateTime = tides_1min$DateTime,
  tide_m   = tides_1min$tide_m,
  mdot_dev = deviations$deviations[, "MiniDOT Focal"]
  # add other sensors here if needed
)

#---- Calculate a rolling SD of temp deviance to dampen variability in the full deployment. ----
# Rolling SD of selected sensor deviation from ensemble mean
# Resolution: win <- 12 * 12  # 12 hours at 5-min resolution
win <- 12 * 60  # 12 hours at 1-min resolution (M2 frequency is 12.4) as per deviance data)
library(zoo)
devs <- tdevs$mdot_dev
roll_tdev_sd <- rollapply(devs, width = win, FUN = sd, na.rm = TRUE, fill = NA)

#----  Plot temperature variance vs. tidal range  ----
plot_tvar_vs_tides( roll_tdev_sd, tides_1min )

#---- Quantify the correlation after June when stratification appears to exist ----
idx <- plot_tdev$Date >= as.POSIXct("2025-07-15")
cor( plot_tides$t_range[idx], plot_tdev$sd[idx], use = "complete.obs")

#--------------------Cross-correlation analysis ------------------------------
# Estimate tidal propagation lag to focal site. Requires tide predictions from above 

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

str( tdevs$mdot_dev )
str( tides_1min )

# Focal site ... 
plot_prop_lag( mdot_foc, tides_1min )
  # Peak negative lag ~240 observations = 4 hours at 1-min resolution
  # peak_lag_obs <- 240
  # peak_lag_hrs <- peak_lag_obs / 60
  # cat(sprintf("Tidal propagation lag: ~%.1f hours\n", peak_lag_hrs))

# reference site  ... 
plot_prop_lag( mdot_ref, tides_1min )
# Peak negative lag ~240 observations = 4 hours at 1-min resolution
# peak_lag_obs <- 240
# peak_lag_hrs <- peak_lag_obs / 60
# cat(sprintf("Tidal propagation lag: ~%.1f hours\n", peak_lag_hrs))


#---- Interpreting the CCF results 

mean( mdot_ref$Temp )
mean( mdot_foc$Temp )



# fin.
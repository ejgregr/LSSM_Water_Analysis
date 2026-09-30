# #============================================================================
# Script:  data_validation.R
# Purpose: Assorted routines to evaluate the various Broughton data sets.
#          This is useful for correctly parameterising the growth model, as
#          well as informing any box model down the road. 
# Includes:
#   1) Coherence across temperature sensors
#   2) Coherence across salinity sensors
#   3) Temperature agreement between sensors and discrete samples
#   4) Effect of depth on temperatures
#   5) Tidal effects on mooring temps, and characterization of Canoe Pass flow
#============================================================================

# *** Ensure all data trimmed to deployment period and maintenance windows ***

#------- Part 1a - Check coherence across temperature sensors --------------------
# Initial concern was that Mooring temps were initially unreasonably high. 
# Led to sorting out problems with the data loading.
# That and and trimming to maintenance windows removed the temp anomalies.
max(mdot_foc$Temp ); max(DST_foc1$Temp ); max(DST_foc2$Temp )
max(mdot_ref$Temp ); max(DST_ref1$Temp ); max(DST_ref2$Temp )

# calculate deviations ... 
calc_deviations <- function(series_list) {
  
  longest    <- series_list[[which.max(sapply(series_list, function(s) nrow(s$df)))]]
  t_grid     <- longest$df$DateTime
  
  interp_mat <- do.call(cbind, lapply(series_list, function(s) {
    approx(s$df$DateTime, s$df[[s$var]], xout = t_grid, method = "linear")$y
  }))
  
  # Only compute ensemble mean where >= 2 series have data
  n_valid    <- rowSums(!is.na(interp_mat))
  ens_mean   <- ifelse(n_valid >= 2, rowMeans(interp_mat, na.rm = TRUE), NA)
  deviations <- sweep(interp_mat, 1, ens_mean, "-")
  colnames(deviations) <- sapply(series_list, function(s) s$label)
  
  list(t_grid     = t_grid,
       deviations = deviations)
}

# Function 2: Plot deviations and return statistics
plot_deviations <- function(dev_obj, metric = "Value",
                            colours = c("steelblue", "darkred", "darkgreen",
                                        "purple", "darkorange")) {
  t_grid     <- dev_obj$t_grid
  deviations <- dev_obj$deviations
  n          <- ncol(deviations)
  ylim       <- range(deviations, na.rm = TRUE)
  
  par(mfrow = c(n, 1), mar = c(2, 4, 1, 1))
  
  for (i in 1:n) {
    plot(t_grid, deviations[, i],
         type = "l", col = colours[i], lwd = 0.8,
         xlim = range(t_grid, na.rm = TRUE),
         ylim = ylim,
         xlab = "", ylab = paste("Deviation (", metric, ")", sep = ""),
         main = colnames(deviations)[i])
    abline(h = 0, lty = 2)
  }
  
  mtext("Date", side = 1, line = 3)
  par(mfrow = c(1, 1))
  
  data.frame(
    Sensor  = colnames(deviations),
    Bias    = round(colMeans(deviations, na.rm = TRUE), 4),
    SD      = round(apply(deviations, 2, sd, na.rm = TRUE), 4),
    RMSE    = round(apply(deviations, 2, function(x) sqrt(mean(x^2, na.rm = TRUE))), 4),
    N_flags = colSums(abs(deviations) > 2 * apply(deviations, 2, sd, na.rm = TRUE),
                      na.rm = TRUE)
  )
}

#--- Check deviations across temperatures 
my_series <- list(
  list(df = mdot_foc,  var = "Temp", label = "MiniDOT Focal"),
  list(df = DST_foc1,  var = "Temp", label = "DST Focal 1"),
  list(df = DST_foc2,  var = "Temp", label = "DST Focal 2")
)

deviations  <- calc_deviations( my_series)
dev_results <- plot_deviations(deviations, metric = "°C")

#----- Spectral analysis to examine periodicity in temperature deviation --------
# Deviation is synchronous among sensors so move forward with mdot only. 
# Next question is why the deviance was low during some time periods. See Part 3 below.

dt_hours <- 5/60  # 5-minute sampling in hours

spec_out <- spec.pgram(deviations[!is.na(deviations[, "mdot_foc"]), "mdot_foc"],
                       spans = c(11, 11),
                       plot  = FALSE)

plot(spec_out$freq / dt_hours, spec_out$spec,
     type = "l", log  = "xy",
     xlab = "Frequency (cycles per hour)",
     ylab = "Spectrum",
     main = "Spectrum of mdot_foc deviations")

# Mark M2 and K1 frequencies
abline(v = 1/12.4206, col = "red",  lty = 2)  # M2
abline(v = 1/23.9345, col = "blue", lty = 2)  # K1
legend("topright", legend = c("M2", "K1"),
       col = c("red", "blue"), lty = 2, bty = "n")

#--> clear signal of M2 forcing, K21 also evident, as is seasonal warming and 
# variable (wind-driven?) mixing intensity. More tide work in Part 3.

#----- Part 1b - Check coherence across Salinity  --------------------
# A function for pair-wise comparisons 
compare_sensors <- function(df1, df2, var, 
                            lab1 = "Sensor 1", lab2 = "Sensor 2", pcolor="blue") {
  
  y_interp <- approx(x      = df2$DateTime,
                     y      = df2[[var]],
                     xout   = df1$DateTime,
                     method = "linear")$y
  
  resid <- df1[[var]] - y_interp
  
  cat(sprintf("Bias: %.3f  SD: %.3f  Range: %.3f to %.3f\n",
              mean(resid, na.rm = TRUE),
              sd(resid,   na.rm = TRUE),
              min(resid,  na.rm = TRUE),
              max(resid,  na.rm = TRUE)))
  
  plot(y_interp, df1[[var]], col = pcolor,
       pch  = 16, cex = 0.3,
       xlab = paste(lab2, var),
       ylab = paste(lab1, var),
       main = paste(lab1, "vs", lab2, "-", var))
  abline(0, 1, lty = 2)
}

#--- Salinity
par(mfrow = c(2, 1))
compare_sensors(DST_ref1, DST_ref2, "Salinity", "DST Ref 1", "DST Ref 2", "darkgreen")
compare_sensors(DST_foc1, DST_foc2, "Salinity", "DST Focal 1", "DST Focal 2")
par(mfrow = c(1, 1))

# Interesting question is whether salinity shows the same tidal/spring-neap 
# signal as temp. If so, would support the tidal mixing interpretation. 
# Worth doing? Value? 

#---- Part 1c - Examine sensors at various times to investigate anomalies ----  
start <- "2025-08-04 09:00:00"
end   <- "2025-09-01 15:00:00"

str(DST_foc1)
# Pinch and plot datasets
x <- DST_foc2[, c("DateTime", "Salinity")]
y <- DST_foc1[, c("DateTime", "Salinity")]

x$Dataset <- "DST Foc2"
y$Dataset <- "DST Foc1"
plot_dat <- rbind(x, y )

two_series_plot( plot_dat, "Salinity", "Salinity", "", sdate = start, edate = end)


#---- Part 2 - Check agreement across between sensors and discrete samples ----
# Helper function to calc bias and make a plot
compare_discrete <- function(discrete_df, sensor_df, var,
                             lab_discrete = "Discrete", lab_sensor = "Sensor") {
  
  # Interpolate sensor onto discrete sample timestamps
  sensor_interp <- approx(x      = sensor_df$DateTime,
                          y      = sensor_df[[var]],
                          xout   = discrete_df$DateTime,
                          method = "linear")$y
  
  resid <- discrete_df[[var]] - sensor_interp
  
  cat(sprintf("Variable: %s\n", var))
  cat(sprintf("Bias: %.3f  SD: %.3f  Range: %.3f to %.3f\n",
              mean(resid, na.rm = TRUE),
              sd(resid,   na.rm = TRUE),
              min(resid,  na.rm = TRUE),
              max(resid,  na.rm = TRUE)))
  
  plot(sensor_interp, discrete_df[[var]],
       pch  = 16, cex = 1.0,
       xlab = paste(lab_sensor, var),
       ylab = paste(lab_discrete, var),
       main = paste(lab_discrete, "vs", lab_sensor, "-", var))
  abline(0, 1, lty = 2)
}

# Prepare the discrete data ... 
x <- clean_data[ clean_data$Station %in% c("CP_In", "Focal", "R3"),]
x <- x[ order( x$Station ), ]
x[ x$Station == "R3", ]$Station <- "Ref"
x[ x$Station != "Ref", ]$Station <- "Foc"

foc_discrete <- x[x$Station == "Foc", ]
ref_discrete <- x[x$Station == "Ref", ]

# --- Part 2a: Temperature. NOTE that between miniDot and StarODDI, the temperature
# coherence analysis showed the MDOT had the least deviance from the overall mean,
# so focus on that here.

# Discrete temp vs. MDOT 
par(mfrow = c(1, 2))
compare_discrete(foc_discrete, mdot_foc, "Temp", "FOC", "Mooring")
compare_discrete(ref_discrete, mdot_ref, "Temp", "REF", "Mooring")
par(mfrow = c(1, 1))

# DONE. There is a BIAS at both sites, with surface temps warmer. 
# 0.77 at the focal site and 0.56 at the reference site. 

# Check an odd temperature spike in late August

mdot_foc$DateTime[which.max(mdot_foc$Temp)]
mdot_ref$DateTime[which.max(mdot_ref$Temp)]

DST_foc1$DateTime[which.max(DST_foc1$Temp)]
DST_foc2$DateTime[which.max(DST_foc2$Temp)]

DST_ref1$DateTime[which.max(DST_ref1$Temp)]
DST_ref2$DateTime[which.max(DST_ref2$Temp)]

#Currently created below
#head(tides)
tides[as.Date(tides$DateTime) == as.Date("2025-08-25"), ]


#---- Part 2c - Check effect of depth on temperature bias. 

# BOX PLOT of DEPTHS
# Combine DST depth data with sensor labels
depth_df <- rbind(
  data.frame(Sensor = "DST_foc1", Depth = DST_foc1$Depth),
  data.frame(Sensor = "DST_foc2", Depth = DST_foc2$Depth),
  data.frame(Sensor = "DST_ref1", Depth = DST_ref1$Depth),
  data.frame(Sensor = "DST_ref2", Depth = DST_ref2$Depth)
)

boxplot(Depth ~ Sensor, data = depth_df,
        xlab = "Sensor", ylab = "Depth (m)",
        main = "DST Sensor Depths",
        col  = c("steelblue", "dodgerblue", "darkgreen", "green3"))
abline(h = 0, lty = 2)

# LINE PLOT of DST FOCAL depths
par(mfrow = c(2, 1), mar = c(2, 4, 2, 1))

plot(DST_foc1$DateTime, DST_foc1$Depth,
     type = "l", col = "steelblue", lwd = 0.8,
     xlab = "", ylab = "Depth (m)",
     main = "DST Focal 1")
abline(h = 0, lty = 2)

plot(DST_foc2$DateTime, DST_foc2$Depth,
     type = "l", col = "dodgerblue", lwd = 0.8,
     xlab = "Date", ylab = "Depth (m)",
     main = "DST Focal 2")
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))
par(mfrow = c(3, 1), mar = c(2, 4, 1, 1))

sens <- DST_foc1
plot(sens$DateTime, sens$Depth,
     type = "l", col = "steelblue", lwd = 0.8,
     xlab = "", ylab = "Depth (m)",
     main = "DST Focal 2")
abline(h = 0, lty = 2)

plot(sens$DateTime, sens$Temp,
     type = "l", col = "darkred", lwd = 0.8,
     xlab = "", ylab = "Temperature (°C)")

plot(sens$DateTime, sens$Salinity,
     type = "l", col = "darkgreen", lwd = 0.8,
     xlab = "Date", ylab = "Salinity (psu)")

par(mfrow = c(1, 1))


#-------------------------Part 3 Tidal data -------------------------------
# Load tidal data and examine role with respect to temperature and flow 
# through Canoe pass. Creates the de-trended tide height temperature plot, 
# and the CCF plots of tide vs. temperature at both mooring sites.

# Load 2025 tide heights for Alert Bay
tide_file <- paste0(source_dir, "/tidalflow/Annual_Predictions_Alert Bay_2025.csv")
tides <- read.csv(tide_file, header = TRUE)
names(tides) <- c("DateTime", "Metres")
tides$DateTime <- as.POSIXct(tides$DateTime, format = "%Y-%m-%dT%H:%M:%S", tz = "America/Vancouver")
tides$Metres   <- as.numeric(as.character(tides$Metres))

# Trim to mooring deployment period
tides <- tides[tides$DateTime >= as.POSIXct(sdate, tz = "America/Vancouver") &
               tides$DateTime <= as.POSIXct(edate, tz = "America/Vancouver"), ]

# Create a 5 min tidal prediction to examine relationship with temperature sensors 
# First need the time grid 
temp_mat <- make_time_grid()

# Now make the tidal predictions
tides_mdot_5min <- predict_tides(tides, temp_mat)


#-------- Plotting focal MDOT Deviance from above with tidal predictions --------

# First build a df with the deviations added to the tidal date
# NB: tdevs = 'tides with temp deviations'
tdevs <- cbind( tides_mdot_5min, deviations )
head( tdevs)
tail( tdevs)

#--- Calculate a rolling SD of temp deviance to dampen the variability in the full deployment.
library(zoo)

# Rolling SD of mdot_foc deviation from ensenble mean
win <- 12.4 * 12  # 12 hours at 5-min resolution
roll_tdev_sd <- rollapply(tdevs$mdot_foc, width = win, FUN = sd, na.rm = TRUE, fill = NA)

#--- and dampen the signal  ... 
daily_tdev_sd <- tapply( roll_tdev_sd,
                   as.Date( tdevs$DateTime ),
                   mean, na.rm = TRUE )

plot_tdev <- data.frame( Date = as.POSIXct(names(daily_tdev_sd)),
                           sd   = as.numeric(daily_tdev_sd) )

#--- Also dampen the tidal data ... 
daily_tide_range <- tapply(tides_mdot_5min$tide_m,
                      as.Date(tides_mdot_5min$DateTime),
                      function(x) diff(range(x, na.rm = TRUE)))

plot_tides <- data.frame( Date    = as.POSIXct(names(daily_tide_range)),
                          t_range = as.numeric(daily_tide_range)
)
# drop last elements as tides have a 0 at the end
plot_tdev  <- plot_tdev [ -nrow(plot_tdev), ]
plot_tides <- plot_tides[ -nrow(plot_tides), ]
tail( plot_tides )

#--- Plot it up
ylim_sd    <- range( plot_tdev$sd,      na.rm = TRUE)
ylim_range <- range( plot_tides$t_range, na.rm = TRUE)

par(mar = c(5, 4, 4, 4) + 0.1)

plot(plot_tides$Date, plot_tides$t_range,
     type = "l", col = "darkorange", lwd = 2,
     xlab = "Date", ylab = "Daily Tidal Range (m)",
     ylim = ylim_range,
     main = "Daily Tidal Range vs Focal MDOT Temperature Variance")

par(new = TRUE)
plot(plot_tdev$Date, plot_tdev$sd,
     type = "l", col = "steelblue", lwd = 2,
     axes = FALSE, xlab = "", ylab = "",
     ylim = ylim_sd)

axis(side = 4)
mtext("Daily Mean Rolling SD (°C)", side = 4, line = 3)

legend("topleft",
       legend = c("Daily Tidal Range (m)", "Daily Mean Rolling SD (°C)"),
       col    = c("darkorange", "steelblue"),
       lty    = 1, lwd = 2, bty = "n")

# Quantify the correlation after June when stratification appears to exist 
idx <- plot_tdev$Date >= as.POSIXct("2025-07-01")
cor( plot_tides$t_range[idx], plot_tdev$sd[idx], use = "complete.obs")

#------- Cross-correlation analysis to establish tidal propagation lag to focal site ---------
# Create a 1 min tidal prediction to examine tidal lag with MDOT focal temp
tides_1min <- predict_tides(tides, mdot_foc)

mdot_cc        <- mdot_foc
mdot_cc$tide   <- tides_1min$tide_m
mdot_cc        <- mdot_cc[complete.cases(mdot_cc[, c("Temp", "tide")]), ]

# Detrend both series to remove seasonal signal
mdot_cc$Temp_dt <- residuals(loess(Temp ~ as.numeric(DateTime), data = mdot_cc, span = 0.1))
mdot_cc$tide_dt <- residuals(loess(tide ~ as.numeric(DateTime), data = mdot_cc, span = 0.1))

# CCF: negative lag = tide leads temperature
ccf(mdot_cc$tide_dt, mdot_cc$Temp_dt,
    lag.max = 360,
    main    = "CCF: Detrended Tide vs Temperature — Focal Site",
    ylab    = "CCF",
    xlab    = "Lag (observations at 1-min resolution)")


# Peak negative lag ~240 observations = 4 hours at 1-min resolution
# peak_lag_obs <- 240
# peak_lag_hrs <- peak_lag_obs / 60
# cat(sprintf("Tidal propagation lag: ~%.1f hours\n", peak_lag_hrs))

# REFERENCE: Interpolate tidal predictions to mdot_ref timestamps
# Create a 1 min tidal prediction to examine tidal lag with MDOT reference emp
tides_1min <- predict_tides(tides, mdot_ref)

mdot_cc        <- mdot_ref
mdot_cc$tide   <- tides_1min$tide_m
mdot_cc        <- mdot_cc[complete.cases(mdot_cc[, c("Temp", "tide")]), ]

# Detrend both series to remove seasonal signal
mdot_cc$Temp_dt <- residuals(loess(Temp ~ as.numeric(DateTime), data = mdot_cc, span = 0.1))
mdot_cc$tide_dt <- residuals(loess(tide ~ as.numeric(DateTime), data = mdot_cc, span = 0.1))

# CCF: negative lag = tide leads temperature
ccf(mdot_cc$tide_dt, mdot_cc$Temp_dt,
    lag.max = 360,
    main    = "CCF: Detrended Tide vs Temperature — Reference Site",
    ylab    = "CCF",
    xlab    = "Lag (observations at 1-min resolution)")

# Peak negative lag ~240 observations = 4 hours at 1-min resolution
# peak_lag_obs <- 240
# peak_lag_hrs <- peak_lag_obs / 60
# cat(sprintf("Tidal propagation lag: ~%.1f hours\n", peak_lag_hrs))


#---- Interpreting the CCF results 

mean( mdot_ref$Temp )
mean( mdot_foc$Temp )



# fin.
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
#   5) Tidal effects on mooring temps and characterization of Canoe Pass flow
#============================================================================

# Support for the analyses 
source( 'C:/Data/Git/LSSM_Water_Analysis/loading_tides.r')

# *** Ensure all data trimmed to deployment period and maintenance windows ***

#---- Support code to examine sensors at various times to investigate anomalies ----  
start <- "2025-08-04 09:00:00"
end   <- "2025-09-01 15:00:00"

str(DST_foc1)
# Pinch and plot datasets
x <- DST_foc2[, c("DateTime", "Salinity")]
y <- DST_foc1[, c("DateTime", "Salinity")]

x$Dataset <- "DST Foc2"
y$Dataset <- "DST Foc1"
plot_dat <- rbind(x, y )

#two_series_plot( plot_dat, "Salinity", "Salinity", "", sdate = start, edate = end)



#------- Part 1 Temperature coherence across sensors --------------------
# Initial concern was that Mooring temps were initially unreasonably high. 
# Led to sorting out problems with the data loading.
# That and and trimming to maintenance windows removed the temp anomalies.
max(mdot_foc$Temp ); max(DST_foc1$Temp ); max(DST_foc2$Temp )
max(mdot_ref$Temp ); max(DST_ref1$Temp ); max(DST_ref2$Temp )

#--- Deviation function - only for n>=3 sensors in the series list
calc_deviations <- function( series_list ) {
  
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

#--- Deviation plot function
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

# Plot temperature variance vs. tidal range 
plot_tvar_vs_tides <- function( rolldevs, tides ) {
  #--- dampen sd and prep df ...
  daily_tdev_sd <- tapply(rolldevs, as.Date(tides$DateTime), mean, na.rm = TRUE)
  
  plot_tdev <- data.frame(Date = as.POSIXct(names(daily_tdev_sd)), sd = as.numeric(daily_tdev_sd))
  
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
  mtext("Daily Mean Rolling °C Standard Deviation", side = 4, line = 3)
  
  legend(
    "topleft",
    legend = c("Daily Tidal Range (m)", "Daily Mean SD"),
    col    = c("darkorange", "steelblue"),
    lty    = 1,
    lwd = 2,
    bty = "n"
  )#, text.font = 2)
  
  return( c( plot_tdev, plot_tides ) )
}

# Deviation is synchronous among sensors so move forward with mdot only. 



#------- Part 2 Spectral analysis of temperature deviations ----
# Examine periodicity in temperature deviation
# Next question is why the deviance was low during some time periods.

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



#------- Part 3 Temperature variance vs. tidal range ------------------------
#--- Check deviations across FOCAL temperatures 
my_series <- list(
  list(df = mdot_foc,  var = "Temp", label = "MiniDOT Focal"),
  list(df = DST_foc1,  var = "Temp", label = "DST Focal 1"),
  list(df = DST_foc2,  var = "Temp", label = "DST Focal 2") 
)

# Calculate deviance for selected sensors 
deviations  <- calc_deviations( my_series )
plot_deviations( deviations )

# Create matching tidal predictions for plotting 
tides_1min <- predict_tides(tides, data.frame(DateTime = deviations$t_grid))

#---- Create deviation|tide frame for analysis
tdevs <- data.frame(
  DateTime = tides_1min$DateTime,
  tide_m   = tides_1min$tide_m,
  mdot_dev = deviations$deviations[, "MiniDOT Focal"]
)


#---- Calculate a rolling SD of ONE sensor to dampen variability in the full deployment. ----
# Rolling SD of selected sensor deviation from ensemble mean
# Resolution: win <- 12 * 12  # 12 hours at 5-min resolution
win <- 12 * 60  # 12 hours at 1-min resolution (M2 frequency is 12.4) as per deviance data)
library(zoo)
devs <- tdevs$mdot_ref
roll_tdev_sd <- rollapply(devs, width = win, FUN = sd, na.rm = TRUE, fill = NA)


#---- Plot temperature range variance vs. tidal range : MDOT, Focal  --------
# AND return the smoothed data for correlation analysis
sm_dev <- plot_tvar_vs_tides(roll_tdev_sd, tides_1min )

#---- Correlations btwn tides and temp variance for different periods
idx <- sm_dev$Date >= as.POSIXct("2025-06-01") # when stratification appears
cor( sm_dev$t_range[idx], sm_dev$sd[idx], use = "complete.obs")

idx <- sm_dev$Date >= as.POSIXct("2025-08-15") # highest corr period
cor( sm_dev$t_range[idx], sm_dev$sd[idx], use = "complete.obs")

#---- Focal-Reference temperature variance comparison. Uses MDOT sensors. ----
# Comparison based on daily temperature ranges.  

m <- merge(mdot_foc[, c("DateTime", "Temp")], mdot_ref[, c("DateTime", "Temp")],
           by = "DateTime", suffixes = c(".foc", ".ref"))

d   <- format(m$DateTime, "%Y-%m-%d")
rng <- function(v) if (sum(!is.na(v)) < 0.9 * 1440) NA else diff(range(v, na.rm = TRUE))
rf  <- tapply(m$Temp.foc, d, rng)
rr  <- tapply(m$Temp.ref, d, rng)
dd  <- as.Date(names(rf))

par(mfrow = c(1, 2), mar = c(4, 4, 2, 1))
plot(rr, rf, pch = 16, col = adjustcolor("black", 0.5),
     xlab = "Reference daily range (°C)", ylab = "Focal daily range (°C)",
     main = "Paired days")
abline(0, 1, lty = 2)

plot(dd, log2(rf / rr), type = "h", col = "grey40",
     xlab = "", ylab = "log2(focal / reference range)", main = "Relative variability")
abline(h = 0, lty = 2)
abline(h = mean(log2(rf / rr), na.rm = TRUE), col = "red")


#---- Make a tidal-temperature range plot for Reference site. ----
# Uses daily T range as 2 sensors are insufficient for deviance from the mean. 

# Daily temperature range for reference site
d_ref  <- format(DST_ref1$DateTime, "%Y-%m-%d")
rng    <- function(v) if (sum(!is.na(v)) < 0.9 * 480) NA else diff(range(v, na.rm = TRUE))
rf_ref <- tapply(DST_ref1$Temp, d_ref, rng)

daily_ref <- data.frame( Date = as.Date( names(rf_ref) ),
                      t_range = as.numeric( rf_ref ))

# Daily tidal range
daily_tide_range <- tapply(tides_1min$tide_m,
                           as.Date(tides_1min$DateTime),
                           function(x) diff(range(x, na.rm = TRUE)))
plot_tides <- data.frame(
  Date    = as.Date(names(daily_tide_range)),
  t_range = as.numeric(daily_tide_range)
)
# Trim last row
plot_tides <- plot_tides[-nrow(plot_tides), ]

# Create the plot in base R
ylim_temp <- range(daily_ref$t_range,  na.rm = TRUE)
ylim_tide <- range(plot_tides$t_range, na.rm = TRUE)

par(mar = c(5, 4, 4, 4) + 0.1)
plot(plot_tides$Date, plot_tides$t_range,
     type = "l", col = "darkorange", lwd = 2,
     xlab = "Date", ylab = "Daily Tidal Range (m)",
     ylim = ylim_tide, main = "Daily Tidal Range vs Reference Temperature Variance")

par(new = TRUE)
plot(daily_ref$Date, daily_ref$t_range,
     type = "l", col = "darkgreen", lwd = 2, axes = FALSE, xlab = "", ylab = "", ylim = ylim_temp)

axis(side = 4)
mtext("Daily Temperature Range (°C)", side = 4, line = 3)

legend("topleft",
       legend = c("Daily Tidal Range (m)", "Daily Temperature Range (°C)"),
       col    = c("darkorange", "darkgreen"), lty    = 1, lwd = 2, bty = "n", text.font = 2)

#---- Correlations btwn tides and temp variance for different periods
idx <- daily_ref$Date <= as.POSIXct("2025-07-15") # BEFORE stratification appears
cor( daily_ref$t_range[idx], plot_tides$t_range[idx], use = "complete.obs")

idx <- daily_ref$Date >= as.POSIXct("2025-08-01") # Stratified, negative corr period
cor( daily_ref$t_range[idx], plot_tides$t_range[idx], use = "complete.obs")



#------ Part 4 Coherence across Salinity  --------------------
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

# Question: does salinity variance show the same spring-neap signal as temp?
# If so, more evidence for tidal mixing interpretation. Value? 


#------ Part 5 Periodicity in Temperature deviation ------------------------
# Spectral analysis of periodicity in temperature deviation.
# Deviation is synchronous among sensors so move forward with mdot only. 
# Next question is why the deviance was low during some time periods. See Part 3 below.

#---- Set resolution of deviation analysis
dt_hours <- 5/60  # 5-minute sampling in hours

# Estimate the spectral density of the deviations. 
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

#---  Cross-correlation analysis to establish tidal propagation lag at focal site ---------
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



#------ Part 6 - Agreement across between sensors & discrete samples ----
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








#----
# fin.
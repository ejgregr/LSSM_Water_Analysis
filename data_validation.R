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
# 11/18: Replaced Alert Bay tidal data with Cedar Island.
# 11/23: Reorganizing, grouping, and adding. sigh. 
#============================================================================

# *** Ensure all data trimmed to deployment period and maintenance windows ***

#----------------------- Functions for sections below --------------------------
#--- Deviation function - only for n>=3 sensors in a pre-defined series list
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
  daily_tide_range <- tapply(tides$Metres, as.Date(tides$DateTime), function(x)
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


#---- Examine pairs of sensors at specified times to investigate anomalies----  

# Select sensors 
x <- DST_foc1[, c("DateTime", "Salinity")]
y <- DST_foc2[, c("DateTime", "Salinity")]

# Build structure 
x$Dataset <- "DST Foc1"
y$Dataset <- "DST Foc2"
plot_dat <- rbind(x, y )

# Temporal window and plot 
tstart <- "2025-08-04 09:00:00"
tend   <- "2025-09-01 15:00:00"
two_series_plot( plot_dat, "Salinity", "Salinity", "", sdate = tstart, edate = tend)

#------- Part 1 Temperature coherence across sensors --------------------
# Initial concern was that Mooring temps were unreasonably high. 
# Led to sorting out problems with the data loading.
# Repaired data loading and trimming to maintenance windows solved main temp anomalies.
max(mdot_foc$Temp ); max(DST_foc1$Temp ); max(DST_foc2$Temp )
max(mdot_ref$Temp ); max(DST_ref1$Temp ); max(DST_ref2$Temp )

#--- Examine deviations across temperature sensors
my_series <- list(
  list(df = mdot_foc,  var = "Temp", label = "MiniDOT Focal"),
  list(df = DST_foc1,  var = "Temp", label = "DST Focal 1"),
  list(df = DST_foc2,  var = "Temp", label = "DST Focal 2")
)

deviations  <- calc_deviations( my_series )
dev_results <- plot_deviations(deviations, metric = "°C")
# Deviation is synchronous among sensors so further validation with mdot only. 

#------- Part 2 Spectral analysis of temperature deviations ----
# Examine periodicity in temperature deviation

# temporal resolution for spectral analysis - based on deviations df.
dt_hours <- 1/60  # 1-minute sampling in hours

x <- deviations$deviations
y <- x[ !is.na( x[, "MiniDOT Focal"]), "MiniDOT Focal"]

spec_out <- spec.pgram(y, spans = c(11, 11),
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

#--> clear signal of M2 forcing, K1 also evident, as is seasonal warming and 
# variable (wind-driven?) mixing intensity. More tide work in Part 3.


#------- Part 3 Temperature vs. tidal range ------------------------

#---- Part 1 of 3 ... 
# Using deviations from above, create corresponding time df for tidal predictions 
tides_1min <- predict_tides(tides, data.frame(DateTime = deviations$t_grid))

#---- Part 2 ... 
# Build tdevs df for analysis, Calculate a rolling SD to dampen variability across deployment.

#---- Create deviation|tide frame for analysis
tdevs <- data.frame(
  DateTime = tides_1min$DateTime,
  Metres   = tides_1min$Metres,
  mdot_dev = deviations$deviations[, "MiniDOT Focal"]
)

# Rolling SD of selected sensor deviation from ensemble mean
# Resolution: win <- 12 * 12  # 12 hours at 5-min resolution
win <- 12 * 60  # 12 hours at 1-min resolution (M2 frequency is 12.4) as per deviance data)
library(zoo)
devs <- tdevs$mdot_dev
roll_tdev_sd <- rollapply(devs, width = win, FUN = sd, na.rm = TRUE, fill = NA)

#Plot temperature range SD (variance) vs. tidal range AND return the smoothed data for correlation analysis
sm_dev <- plot_tvar_vs_tides(roll_tdev_sd, tides_1min )


Part 3 ... 


# Nov 23: Because REF site only has a single sensor, use range instead of SD from above. 
# Plotting SD with smoothed code removed. See the 'old' script. 
#---- Tidal range vs daily temperature range: Reference and Focal sites ----
# Uses daily T range as 2 sensors are insufficient for deviance from the mean.

library(zoo)

# Daily range of a variable; NA if a day has < min_frac of expected samples
daily_range <- function(df, var = "Temp", min_frac = 0.5) {
  dt_sec <- median(diff(as.numeric(df$DateTime)), na.rm = TRUE)
  n_full <- 86400 / dt_sec
  d      <- format(df$DateTime, "%Y-%m-%d")
  r      <- tapply(df[[var]], d, function(v)
    if (sum(!is.na(v)) < min_frac * n_full) NA
    else diff(range(v, na.rm = TRUE)))
  data.frame(Date = as.Date(names(r)), range = as.numeric(r))
}


# Create a tide prediction for the deployment period ... 
tgrid      <- data.frame(DateTime = seq(as.POSIXct(sdate), as.POSIXct(edate), by = "1 min"))
tides_1min <- daily_range( predict_tides(tides, tgrid), var = "Metres" )


k <- 2   # smoothing window (days); keep << 14.8 d spring-neap period

ref_rng  <- daily_range(DST_ref1)
foc_rng  <- daily_range(DST_foc1)
tide_rng <- tides_1min

names(ref_rng)[2]  <- "ref"
names(foc_rng)[2]  <- "foc"
names(tide_rng)[2] <- "tide"

daily <- merge(tide_rng, ref_rng, by = "Date", all = TRUE)
daily <- merge(daily,    foc_rng, by = "Date", all = TRUE)

# Ensure a continuous daily sequence so the rolling window spans real days
all_days <- data.frame(Date = seq(min(daily$Date), max(daily$Date), by = "day"))
daily    <- merge(all_days, daily, by = "Date", all.x = TRUE)

smooth_k <- function(x, k) rollapply(x, width = k, FUN = mean, na.rm = TRUE,
                                     fill = NA, align = "center")
daily$ref_s <- smooth_k(daily$ref, k)
daily$foc_s <- smooth_k(daily$foc, k)

# Plot
ylim_temp <- range(c(daily$ref_s, daily$foc_s), na.rm = TRUE)
ylim_tide <- range(daily$tide, na.rm = TRUE)

par(mfrow = c(1, 1), mar = c(5, 4, 4, 4) + 0.1)
plot(daily$Date, daily$tide,
     type = "l", col = "grey", lwd = 2,
     xlab = "Date", ylab = "Daily Tidal Range (m)", ylim = ylim_tide,
     main = paste0("Daily Tidal Range vs Temperature Range (", k, "-day mean)"))

par(new = TRUE)
plot(daily$Date, daily$ref_s,
     type = "l", col = "orange", lwd = 2,
     axes = FALSE, xlab = "", ylab = "", ylim = ylim_temp)
lines(daily$Date, daily$foc_s, col = "steelblue", lwd = 2)

axis(side = 4)
mtext("Daily Temperature Range (°C)", side = 4, line = 3)

legend("topleft",
       legend = c("Tidal range", "Reference site", "Focal site"),
       col = c("grey", "orange", "steelblue"),
       lty = 1, lwd = 2, bty = "n", text.font = 2)


#---- Correlations (date-matched)
pre  <- daily$Date <= as.Date("2025-07-15")
post <- daily$Date >= as.Date("2025-08-01")
cor(daily$tide[pre],  daily$ref[pre],  use = "complete.obs")
cor(daily$tide[post], daily$ref[post], use = "complete.obs")
cor(daily$tide[pre],  daily$foc[pre],  use = "complete.obs")
cor(daily$tide[post], daily$foc[post], use = "complete.obs")


#---- Correlations btwn tides and temp variance for different periods
idx <- sm_dev$Date >= as.POSIXct("2025-06-01") # when stratification appears
cor( sm_dev$t_range[idx], sm_dev$sd[idx], use = "complete.obs")

idx <- sm_dev$Date >= as.POSIXct("2025-08-15") # highest corr period
cor( sm_dev$t_range[idx], sm_dev$sd[idx], use = "complete.obs")

#---- Correlations btwn tides and temp variance for different periods
idx <- daily_ref$Date <= as.POSIXct("2025-07-15") # BEFORE stratification appears
cor( daily_ref$t_range[idx], plot_tides$t_range[idx], use = "complete.obs")

idx <- daily_ref$Date >= as.POSIXct("2025-08-01") # Stratified, negative corr period
cor( daily_ref$t_range[idx], plot_tides$t_range[idx], use = "complete.obs")


#---- Compare variance in temperature ranges between Focal and Reference sites. Uses MDOT sensors. ----
# Range rather than deviance because its a pair-wise comparision.

# starts with loaded sensor data ... 
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



#------ Part 4 Comparisons across Sensors --------------------
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

#--- Temperature
par(mfrow = c(2, 1))
compare_sensors(DST_ref1, DST_ref2, "Temp", "DST Ref 1", "DST Ref 2", "darkgreen")
compare_sensors(DST_foc1, DST_foc2, "Temp", "DST Focal 1", "DST Focal 2")
par(mfrow = c(1, 1))

#--- Depth 


#------ Part 5 Cross-correlation analysis to establish tidal propagation lag at focal site ---------

# Created fro spectral analysis above.
str(tides_1min) 

mdot_cc        <- mdot_foc
mdot_cc$tide   <- tides_1min$Metres
mdot_cc        <- mdot_cc[complete.cases(mdot_cc[, c("Temp", "tide")]), ]

# Detrend both series to remove seasonal signal





#------ Part 6 - Agreement across between sensors & discrete samples ----
# Helper function to calc bias and make a plot
# var is one of: Temp, Salt, pCO2
# interpolates var time onto discrete time
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

# Prepare the discrete data (created by discrete_sample_loading.r)  
# NOTES: There is 1 CP sampled named "Focal" in the source spreadsheet
#        R3 is the 'In' sample at the reference site. 
x <- disc_data[ disc_data$Station %in% c("CP_In", "Focal", "R3"),]
x <- x[ order( x$Station ), ]
x[ x$Station == "R3", ]$Station <- "Ref"
x[ x$Station != "Ref", ]$Station <- "Foc"

foc_discrete <- x[x$Station == "Foc", ]
ref_discrete <- x[x$Station == "Ref", ]

str(x)

# --- Plot a series of bias plots and print the biases to the console 
# Use StarODDI so can check salinity as well. 

par(mfrow = c(1, 2),mar = c(5, 4, 4, 2))

# Discrete temp vs. MDOT 
compare_discrete(foc_discrete, DST_foc1, "Temp", "Focal", "Mooring")
compare_discrete(ref_discrete, DST_ref1, "Temp", "Reference", "Mooring")
# Shows a BIAS at both sites, with surface temps warmer. 
# 0.77 at the focal site and 0.56 at the reference site. 

compare_discrete(foc_discrete, DST_foc1, "Salinity", "Focal", "Mooring")
compare_discrete(ref_discrete, DST_ref1, "Salinity", "Reference", "Mooring")

# Discrete pH vs. CO2Pro
# Here need to remove REF discrete prior to July 11 ... 
compare_discrete(foc_discrete, CO2_foc, "pCO2", "Focal", "Mooring")
compare_discrete(ref_discrete, CO2_ref, "pCO2", "Reference", "Mooring")
par(mfrow = c(1, 1))


#---- Part 2c - Check effect of depth on temperature bias. 

compare_sensors( DST_ref1, DST_ref2, "Temp", "Ref1", "Ref2" )

head( DST_ref1)
head( DST_ref2)

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


# Interpolate DST_ref2 onto DST_ref1 timestamps
ref2_interp <- approx(x      = DST_ref2$DateTime,
                      y      = DST_ref2$Temp,
                      xout   = DST_ref1$DateTime,
                      method = "linear")$y

# Temperature difference (shallow - deep)
temp_diff <- DST_ref1$Temp - ref2_interp

# Summary statistics
cat(sprintf("Mean bias:  %.3f °C\n", mean(temp_diff, na.rm = TRUE)))
cat(sprintf("SD:         %.3f °C\n", sd(temp_diff,   na.rm = TRUE)))
cat(sprintf("Range:      %.3f to %.3f °C\n", 
            min(temp_diff, na.rm = TRUE), 
            max(temp_diff, na.rm = TRUE)))

# Time series plot
par(mfrow = c(2, 1), mar = c(2, 4, 1, 1))

plot(DST_ref1$DateTime, temp_diff,
     type = "l", col = "steelblue", lwd = 0.8,
     xlab = "", ylab = "Temp difference (°C)",
     main = "Shallow minus Deep temperature (DST Ref1 - Ref2)")
abline(h = 0,                          lty = 2)
abline(h = mean(temp_diff, na.rm = TRUE), col = "red", lwd = 1.5)

# Rolling daily mean to show seasonal trend
daily_diff <- tapply(temp_diff,
                     as.Date(DST_ref1$DateTime),
                     mean, na.rm = TRUE)

plot(as.Date(names(daily_diff)), as.numeric(daily_diff),
     type = "l", col = "darkred", lwd = 1.5,
     xlab = "Date", ylab = "Daily mean diff (°C)",
     main = "Daily mean temperature difference")
abline(h = 0, lty = 2)

par(mfrow = c(1, 1))









#----
# fin.


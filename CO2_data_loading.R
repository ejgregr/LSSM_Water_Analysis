#### CO2 Pro  DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 30, 2025
############################################################################

# none yet.


#------ Data loading section ----
# Wiley has inconsistent naming for the monthly files so takes a few steps
# We are using the matlab files, not the text files. 

# get all the files ... 
co2_files <- list.files( paste0( source_dir, '/CO2Pro' ))

# separate into focal and reference moorings ... 
focal_files <- co2_files[ grepl("kelp", co2_files, ignore.case = TRUE) &
                          !grepl("matlab", co2_files, ignore.case = TRUE) ]

ref_files  <- co2_files[ grepl("ref", co2_files, ignore.case = TRUE) &
                         !grepl("matlab", co2_files, ignore.case = TRUE) ]

# extend file names with path  ... 
focal_files_full <- file.path( paste0( source_dir, '/CO2Pro/', focal_files) )
ref_files_full   <- file.path( paste0( source_dir, '/CO2Pro/', ref_files) )

# define the columns to keep ... 
cols_to_keep <- c("Year", "Month", "Day", "Hour", "Minute", "Second", "CO2")

# final data loading ... 
focal_dat <- load_and_bind( focal_files_full, cols_to_keep )
ref_dat   <- load_and_bind( ref_files_full, cols_to_keep )

difftime( focal_dat[1,]$Timestamp, focal_dat[ dim(focal_dat)[[1]], ]$Timestamp )



# Utility to show the header row in each of the file sets.
for (f in ref_files_full) {
  cat("\n", f, ":\n")           # print the filename
  cat(readLines(f, n = 1), "\n")  # print only the first line
}

#---- Data visualization ---- 

# To show CO2 from both moorings add columns with the dataset names and rbind them
focal_dat$Dataset <- "Focal"
ref_dat$Dataset  <- "Reference"
plot_dat <- rbind(focal_dat, ref_dat)

# This shows the early period where the reference CO2Pro was too tight. 
two_series_plot( plot_dat, "CO2", "pCO2 (μatm)", "A title" )

# Remove the offending bit of the time series
ref_dat <- ref_dat[ ref_dat$Timestamp > as.POSIXct("2025-07-15", tz = "UTC"), ]

# Plot it again.
plot_dat <- rbind(focal_dat, ref_dat)
two_series_plot( plot_dat, "CO2", "pCO2 (μatm)", "A title" )

# Now save the focal data to a different df, and shorten to match reference.
# This now sets us up for a comparative study where possible, and also 
# for looking at the full timeseries of the focal site
full_focal_dat <- focal_dat

# Remove the offending ref bit from the focal time series
focal_dat <- focal_dat[ focal_dat$Timestamp > as.POSIXct("2025-07-15", tz = "UTC"), ]

# Plot it again.
plot_dat <- rbind(focal_dat, ref_dat)
full_plot( plot_dat )

# Lets have a quick look at the difference
#--> Check to ensure merge() is not creating data ... 
diff_dat <- merge(
  focal_dat[, c("Timestamp", "CO2")],
  ref_dat[,   c("Timestamp", "CO2")],
  by = "Timestamp",
  suffixes = c("_focal", "_ref")
)

# Lets look at just a week of data ... 
f_dat <- focal_dat[ focal_dat$Timestamp > as.POSIXct("2025-07-15", tz = "UTC") & 
                    focal_dat$Timestamp < as.POSIXct("2025-07-22", tz = "UTC"), ]

r_dat <- ref_dat[ ref_dat$Timestamp > as.POSIXct("2025-07-15", tz = "UTC") & 
                  ref_dat$Timestamp < as.POSIXct("2025-07-22", tz = "UTC"), ]

# Plot it ... 
plot_dat <- rbind(f_dat, r_dat)
str(plot_dat)
two_series_plot( plot_dat, "CO2", "pCO2 (μatm)", "Week of CO2 values for Focal and Reference mooring" )



# now add the difference column and plot it
diff_dat$CO2_diff <- diff_dat$CO2_focal - diff_dat$CO2_ref
plot_diff( diff_dat )

###---> Stop cuz somehow (likely bc of the merge above) there are duplicates in diff_dat.

# Lets try and have a quick look at some variability. 

focal_hr  <- hourly_stats( focal_dat )
focal_day <- daily_stats( focal_dat )

# For daily, sfact=1 and x=Date, as well as title changes
sfact <- 100
#ggplot(focal_hr, aes(x = Hour, y = CO2_mean)) +
ggplot(focal_hr, aes(x = Hour, y = CO2_mean)) +
  geom_ribbon(aes(ymin = CO2_mean - sfact*CO2_sd, ymax = CO2_mean + sfact*CO2_sd),
              fill = "gray20", alpha = 0.4) +
  geom_line(color = "blue", linewidth = .5) +
  scale_y_log10() +
  labs(
    x = "Time (Hourly)",
    y = "CO₂ (mean ± 1 SD)",
    title = "Hourly CO₂ Concentration with Variability"
  ) +
  theme_bw()

###---> Stop here. 




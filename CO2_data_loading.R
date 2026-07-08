#### CO2 Pro  DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 30, 2025
############################################################################
# none yet.
############################################################################

### OUTPUTS are CO2_focal and CO2_ref.

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

# Utility to show the header row in each of the file sets.
for (f in ref_files_full) {
  cat("\n", f, ":\n")           # print the filename
  cat(readLines(f, n = 1), "\n")  # print only the first line
}

# define the columns to keep ... 
cols_to_keep <- c("Year", "Month", "Day", "Hour", "Minute", "Second", "CO2")

# final data loading ... 
CO2_focal <- load_and_bind( focal_files_full, cols_to_keep )
CO2_ref   <- load_and_bind( ref_files_full, cols_to_keep )

difftime( CO2_focal[1,]$Timestamp, CO2_focal[ dim(CO2_focal)[[1]], ]$Timestamp )

#--- CAN STOP HERE ---








#---- Data visualization ---- 

# Show CO2 from both moorings add columns with the dataset names and rbind them
CO2_focal$Dataset <- "Focal"
CO2_ref$Dataset   <- "Reference"
plot_dat <- rbind(CO2_focal, CO2_ref)

# This shows the early period where the reference CO2Pro was too tight. 
two_series_plot( plot_dat, "CO2", "pCO2 (μatm)", "Full pCO2 Timeseries from Focal and Reference Moorings" )

# Remove the offending bit of the REFERENCE time series
CO2_ref <- CO2_ref[ CO2_ref$Timestamp > as.POSIXct("2025-07-15", tz = "UTC"), ]

# Plot it again.
plot_dat <- rbind(CO2_focal, CO2_ref)
two_series_plot( plot_dat, "CO2", "pCO2 (μatm)", "A title" )

# Now save the focal data to a different df, and shorten to match reference.
# This now sets us up for a comparative study where possible, and also 
# for looking at the full timeseries of the focal site
full_focal_dat <- CO2_focal

# Remove the offending ref bit from the focal time series
CO2_focal <- CO2_focal[ CO2_focal$Timestamp > as.POSIXct("2025-07-15", tz = "UTC"), ]

# Plot it again.
plot_dat <- rbind(CO2_focal, CO2_ref)
two_series_plot( plot_dat, "CO2", "pCO2 (μatm)", "A title" )


#--- Diagnostic - Noted below merge may be duplicating data somehow ----
#  Have a look at the difference to ensure merge() is not creating data. 
diff_dat <- merge(
  CO2_focal[, c("Timestamp", "CO2")],
  CO2_ref[,   c("Timestamp", "CO2")],
  by = "Timestamp",
  suffixes = c("_focal", "_ref")
)

# Lets look at just a week of data ... 
f_dat <- CO2_focal[ CO2_focal$Timestamp > as.POSIXct("2025-07-15", tz = "UTC") & 
                    CO2_focal$Timestamp < as.POSIXct("2025-07-22", tz = "UTC"), ]

r_dat <- CO2_ref[ CO2_ref$Timestamp > as.POSIXct("2025-07-15", tz = "UTC") & 
                  CO2_ref$Timestamp < as.POSIXct("2025-07-22", tz = "UTC"), ]

d_dat <- diff_dat[ diff_dat$Timestamp > as.POSIXct("2025-07-15", tz = "UTC") & 
                   diff_dat$Timestamp < as.POSIXct("2025-07-22", tz = "UTC"), ]

str(f_dat)
str(r_dat)
str(d_dat) #<--- The merge() is somehow adding data. 

###---> Stop cuz somehow (likely bc of the merge above) there are duplicates in diff_dat.

# Lets try and have a quick look at some variability. 

focal_hr  <- hourly_stats( CO2_focal )
focal_day <-  daily_stats( CO2_focal )

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

# NOTE: The above just shows that the hour to hour variability far exceeds the hourly SD

###---> Stop here. 



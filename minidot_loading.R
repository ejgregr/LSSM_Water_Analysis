#### MINIDOT DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 30, 2025
############################################################################----
# NOTES:
# none yet.
############################################################################

# MiniDot includes temperature and DO
# 4 sensor folders. 2 for each mooring.
# Each folder has a txt file for each day. There are some summary files

#------ Data LOADING section ----
# Renamed folders based on mooring. 
# Folders contain two summary files. Simpler to use the tab-delimited ones, with no header.

# Build file names ... 
mdot_dirs <- list.files( paste0( source_dir, '/minidot' ))
mdot_all  <- list.files( paste0( source_dir, '/minidot/', mdot_dirs ))

mdot_focal <- mdot_all[ grepl("kelp", mdot_all, ignore.case = TRUE) ]
mdot_ref   <- mdot_all[ grepl("ref", mdot_all, ignore.case = TRUE) ]
fnames <- c(mdot_focal, mdot_ref )

mdot_foc_full <- file.path( paste0( source_dir, '/minidot/', mdot_dirs, '/', fnames) )

ref_files <- mdot_foc_full[ grepl("ref", mdot_foc_full, ignore.case = TRUE) ]
focal_files <- mdot_foc_full[ grepl("focal", mdot_foc_full, ignore.case = TRUE) ]

# Build the focal site dataframe ... 
mdot_focal_dat <- rbind( 
  read.delim(focal_files[1], header = FALSE, check.names = FALSE, stringsAsFactors = FALSE),
  read.delim(focal_files[2], header = FALSE, check.names = FALSE, stringsAsFactors = FALSE) )
  
mdot_ref_dat <- rbind( 
  read.delim(ref_files[1], header = FALSE, check.names = FALSE, stringsAsFactors = FALSE),
  read.delim(ref_files[2], header = FALSE, check.names = FALSE, stringsAsFactors = FALSE) )

# Assign column names ... 
mini_head <- c( "Unix_date", "Battery", "Temp", "DO", "DO_sat", "Q")
names( mdot_focal_dat ) <- mini_head
names( mdot_ref_dat ) <- mini_head

# And translate the date to POSIXct
mdot_ref_dat$Timestamp   <-as.POSIXct( mdot_ref_dat$Unix_date )
mdot_focal_dat$Timestamp <-as.POSIXct( mdot_focal_dat$Unix_date )


#---- Show all data ----
# Merge by Timestamp
merged <- merge(
  mdot_ref_dat[, c("Timestamp", "Temp")],
  mdot_focal_dat[, c("Timestamp", "Temp")],
  by = "Timestamp",
  suffixes = c("_ref", "_focal")
)

ggplot(merged, aes(x = Timestamp)) +
  geom_line(aes(y = Temp_ref, color = "Reference")) +
  geom_line(aes(y = Temp_focal, color = "Focal")) +
  labs(
    x = "Time",
    y = "Temperature (°C)",
    color = "Sensor",
    title = "Temperature Over Time: Reference vs Focal"
  ) +
  scale_color_manual(values = c("Reference" = "blue", "Focal" = "red")) +
  theme_bw()

#------ Data CLEANING section ----

#---- First show all data ----

# Merge by Timestamp
merged <- merge(
  mdot_ref_dat[, c("Timestamp", "Temp")],
  mdot_focal_dat[, c("Timestamp", "Temp")],
  by = "Timestamp",
  suffixes = c("_ref", "_focal")
)

ggplot(merged, aes(x = Timestamp)) +
  geom_line(aes(y = Temp_ref, color = "Reference")) +
  geom_line(aes(y = Temp_focal, color = "Focal")) +
  labs(
    x = "Time",
    y = "Temperature (°C)",
    color = "Sensor",
    title = "Temperature Over Time: Reference vs Focal"
  ) +
  scale_color_manual(values = c("Reference" = "blue", "Focal" = "red")) +
  theme_bw()

#----- Remove times when sensors were out of the water. ----
# These are at the start, 
# end and during mooring service. So:
# Drop both deployment and recovery days (pre May 25, and post Sept 16), 
# and also drop the service days: Jun 12, July 10, and Aug 25th. 
# These data clips can be refined if needed to recover a few more hours of data.

start_date <- as.Date("2025-05-25")
end_date   <- as.Date("2025-09-16")
start_date <- as.Date("2025-05-25")
end_date   <- as.Date("2025-09-16")
service_days <- as.Date(c("2025-06-12", "2025-07-10", "2025-08-25"))

mdot_ref_dat   <- trim_dates(mdot_ref_dat)
mdot_focal_dat <- trim_dates(mdot_focal_dat)

# Looking at this range, is it possible the Ref mooring was dry in late May because of tides?
# Examine the entire time series to see what those other peaks are. i.e., can we
# confirm the date of the June mooring maintenance? If the 12th, then what's 
# going on at the reference site on the 10th? Someone pull it out or what?

plot_range(merged, "2025-05-25", "2025-06-15")


### FIN.
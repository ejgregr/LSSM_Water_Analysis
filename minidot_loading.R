#### MINIDOT DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 30, 2025
################################################################################
# NOTES:
# Temperature is in C; DO is mg/l, and DO_sat is a %
# 2026/07/07: 2nd iteration of data loading simplified to use catenated files,
#   except see below. Fuck. 
################################################################################

# MiniDot includes temperature and DO
# 4 sensor folders. 2 for each mooring, as sensors were replaced as part of July 10 site visit.
# 2nd iteration of data loading now uses the consolidated files in each folder.
# HOWEVER, there is a gap in the consolidated data from 06/12 to 07/10. Fuck. 
# ---> Rebuild just this section from the daily text files. Unclear if 
# this is the same place we would have ended up if just removing duplicates. Fuck. 

#------ Data LOADING section ----
# Mooring name added to sensor IDs as part of folder names.
# Each folder contains a summary file (CAT.txt) containing the catenated values from daily text files.

#--- Deal with the CAT files:
# There are a total of 4 CAT files. Now referenced here explicitly instead of the 
# convoluted file access code in earlier version of this script.  
mdot_dirs <- list.files( paste0( source_dir, '/minidot' ))
cat_files <- paste0( source_dir, '/minidot/', mdot_dirs , '/CAT.txt' )

ref_cat <- cat_files[ grepl("Ref", cat_files ) ]
foc_cat <- cat_files[ grepl("focal", cat_files ) ]

#--- Deal with the time gap in the CAT files by catenating the necessary daily files
# Some manipulation of the files adjacent to Jul 10, the site visit, was necessary
dailies <- c("2025-06-12", "2025-06-13", "2025-06-14", "2025-06-15", "2025-06-16", "2025-06-17", "2025-06-18",
             "2025-06-19", "2025-06-20", "2025-06-21", "2025-06-22", "2025-06-23", "2025-06-24", "2025-06-25",
             "2025-06-26", "2025-06-27", "2025-06-28", "2025-06-29", "2025-06-30", "2025-07-01", "2025-07-02",
             "2025-07-03", "2025-07-04", "2025-07-05", "2025-07-06", "2025-07-07", "2025-07-08", "2025-07-09",
             "2025-07-10" )

d <- paste0( source_dir, '/minidot/focal1-315342/')
foc_files <- list.files( d )
foc_files <-  paste0( d, foc_files[grepl(paste(dailies, collapse = "|"), foc_files)] )

d <- paste0( source_dir, '/minidot/Ref1-801016/')
ref_files <- list.files( d )
ref_files <- paste0( d, ref_files[grepl(paste(dailies, collapse = "|"), ref_files)] )

foc_patch <- read_txts( foc_files )
ref_patch <- read_txts( ref_files )

# Build the focal site dataframe ... 
mdot_ref <- read_mdot( ref_cat )
mdot_foc <- read_mdot( foc_cat )
 
mdot_ref <- rbind(ref_patch, mdot_ref[, !names(mdot_ref) %in% "DO_sat"])
mdot_foc <- rbind(foc_patch, mdot_foc[, !names(mdot_foc) %in% "DO_sat"])

mdot_ref <- mdot_ref[order(mdot_ref$DateTime), ]
mdot_foc <- mdot_foc[order(mdot_foc$DateTime), ]

# Remove duplicates 
mdot_ref <- mdot_ref[!duplicated(mdot_ref$DateTime), ]
mdot_foc <- mdot_foc[!duplicated(mdot_foc$DateTime), ]

# Removed manually identified maintenance periods

# JUNE 10:
mdot_foc <- mdot_foc[!(mdot_foc$DateTime >= as.POSIXct("2025-07-10 20:18:00", tz = "UTC") &
                       mdot_foc$DateTime <= as.POSIXct("2025-07-11 15:00:00", tz = "UTC")), ]
mdot_ref <- mdot_ref[!(mdot_ref$DateTime >= as.POSIXct("2025-07-10 17:08:00", tz = "UTC") &
                       mdot_ref$DateTime <= as.POSIXct("2025-07-10 19:16:00", tz = "UTC")), ]

# Create an insert a gap row to avoid connecting points on opposite sides of the gaps
gap_row <- mdot_ref[1, ]
gap_row$Temp <- NA
gap_row$DO   <- NA

# Focal
gap_row$DateTime <- as.POSIXct("2025-07-10 23:00:00", tz = "UTC")
mdot_foc <- rbind(mdot_foc, gap_row)
mdot_foc <- mdot_foc[order(mdot_foc$DateTime), ]

# Reference
gap_row$DateTime <- as.POSIXct("2025-07-10 18:00:00", tz = "UTC")
mdot_ref <- rbind(mdot_ref, gap_row)
mdot_ref <- mdot_ref[order(mdot_ref$DateTime), ]



#---- Show data for selected date range ---- 

sdate <- "2025-04-25" # The day after deployment
edate <- "2025-09-15" # The day before mooring recovery

#test dates
sdate <- "2025-07-10"
edate <- "2025-07-12"

ref <- mdot_ref[mdot_ref$DateTime >= as.POSIXct(sdate, tz = "UTC") &
                mdot_ref$DateTime <= as.POSIXct(edate, tz = "UTC"), ]

foc <- mdot_foc[mdot_foc$DateTime >= as.POSIXct(sdate, tz = "UTC") &
                mdot_foc$DateTime <= as.POSIXct(edate, tz = "UTC"), ]


# Plot raw temperature data from both sensors 

# Set a common temperature range
ylim_range <- range(c(foc$Temp, ref$Temp), na.rm = TRUE)

plot(foc$Temp ~ foc$DateTime,
     type = "l", xlab = "DateTime", ylab = "Temperature", col = "steelblue",
     ylim = ylim_range, main = "MiniDOT Temperatures - Raw")
par(new = TRUE)
plot(ref$Temp ~ ref$DateTime,
     type = "l", xlab = "", ylab = "", axes = FALSE, col = "darkorange", 
     ylim = ylim_range )
legend("topleft",
       legend = c("Focal", "Reference"),
       col    = c("steelblue", "darkorange"),
       lty    = 1, bty = "n")

#---- Manual removal of mooring maintenance data ... 

# FOC out at 2025-07-10 20:24:00
# FOC in  at 2025-07-11 15:00:00

mdot_foc <- mdot_foc[!(mdot_foc$DateTime >= as.POSIXct("2025-07-10 20:24:00", tz = "UTC") &
                       mdot_foc$DateTime <= as.POSIXct("2025-07-11 15:00:00", tz = "UTC")), ]

mdot_ref <- mdot_ref[!(mdot_ref$DateTime >= as.POSIXct("2025-07-10 17:45:00", tz = "UTC") &
                       mdot_ref$DateTime <= as.POSIXct("2025-07-10 19:16:00", tz = "UTC")), ]

# REF out at 2025-07-10 17:45:00
# REF in  at 2025-07-10 19:16:00



#------ Data VALIDATION section ----
# Concern is that mooring temperatures are unreasonably high at certain periods.
sum(mdot_foc$Temp > 23 )
sum(mdot_ref$Temp > 23 )

# Investigation led to finding some data out of order in the reference data, and 
# duplicate timestamps in both data sets. This seems to have something to do with 
# the July 10 mooring service. 

# Plotting the DateTime index vs DateTime value shows non-monotonic sequences in the reference data
dat <- ref
plot(seq_len(nrow(dat)), dat$DateTime,
     pch = ".", xlab = "Row index", ylab = "DateTime", col = "steelblue",
     main = "DateTime sequence check - Reference")

# Checking for duplicate timestamps 
sum(duplicated(mdot_focal$DateTime))
sum(duplicated(mdot_ref$DateTime))

mdot_focal <- mdot_focal[!duplicated(mdot_focal$Unix_date), ]



# See about fixing the mdot data. NOTE reordering is not the solution


# Plot unique temperature data from both sensors 
plot(foc$Temp ~ foc$DateTime,
     type = "l", xlab = "DateTime", ylab = "Temperature", col = "steelblue",
     main = "MiniDOT Temperatures - Duplicates removed")
par(new = TRUE)
plot(ref$Temp ~ ref$DateTime,
     type = "l", xlab = "", ylab = "", axes = FALSE, col = "darkorange")
legend("topleft",
       legend = c("Focal", "Reference"),
       col    = c("steelblue", "darkorange"),
       lty    = 1, bty = "n")










# Add tide on secondary axis
par(new = TRUE)
plot(tpred_df$DateTime, tpred_df$fit,
     type = "l", col = "darkorange",
     axes = FALSE, xlab = "", ylab = "")
axis(side = 4)
mtext("Tide Height (m)", side = 4, line = 3)



plot(mdot_ref$DateTime, mdot_ref$Temp,
     type = "l", col = "steelblue",
     xlab = "Date", ylab = "Temperature (°C)",
     main = "Temperature - Reference Site")








#---- First show all data ----

# Merge by Timestamp
merged <- merge(
  mdot_ref[, c("Timestamp", "Temp")],
  mdot_focal[, c("Timestamp", "Temp")],
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
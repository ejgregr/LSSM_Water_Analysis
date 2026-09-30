#===============================================================================
# Script:  minidot_loading.R
# Purpose: Load and visualise Minidot data (T and DO) data provided by Wiley
# Created: Nov 2025
# NOTES: Significant bits of this code, particularly string processing, were provided by ChatGPT. 
#   Temperature is in C; DO is mg/l, and DO_sat is a %
#===============================================================================
# Updates:
# 2026/07/07: Simplified to use concatenated files, except see below. Sucks this is a bit faffy
# 2026/07/08: Data trimming cleaned up and consistent
#===============================================================================

# MiniDot includes temperature and DO
# 4 sensor folders. 2 for each mooring; sensors were replaced as part of July 10 site visit.
# 2nd iteration of data loading now uses the consolidated files in each folder.

# Source data description
#  focal1-315342/Cat.TXT : 04/16 to 06/12 (deploy to first site visit)
#  focal2-647102/Cat.TXT : 07/10 to 10/22 (second visit to beyond removal)
#
#  Ref1-801016/Cat.TXT   : 04/16 to 06/12 (deploy to first site visit)
#  Ref2-888141/Cat.TXT   : 05/28 to 10/22 (looks reliable only after 07/10)

# SADLY, Concatenated files have a gap in the data from 06/12 to 07/10. Fuck. 
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

#-----  Hand-build the MDOT dataframes around the June maintenance.
#   Necessary cuz removing duplicates doesn't guarantee corrects ones dropped.  

# REF mooring first ... 
x <- read_mdot( ref_cat[1] )
start <- "2025-04-01 12:00:00" # include full beginning 
end   <- "2025-06-12 14:00:00" # June pull up
mdot_ref <- x[ x$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
               x$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]

# add the patch
x <- ref_patch
start <- "2025-06-12 16:30:00" # from June redeployment  
end   <- "2025-07-10 10:00:00" # to July pull up
x <- x[ x$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
        x$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
mdot_ref <- rbind( mdot_ref, x )

# add the final Cat file
x <- read_mdot( ref_cat[2] )
start <- "2025-07-10 13:00:00" # after July re-deployment 
end   <- "2025-12-12 12:00:00" # to end of timeseries 
x <- x[ x$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
        x$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
x <- x[, !names(x) %in% "DO_sat"]
mdot_ref <- rbind( mdot_ref, x)

# Trim to deployment dates
mdot_ref <- trim_deployment( mdot_ref )

# And Trim the weird Aug blip
ref_aug25_start <- "2025-08-25 12:30:00"
ref_aug25_end   <- "2025-08-25 14:00:00"
mdot_ref <- mdot_ref[!(mdot_ref$DateTime >= as.POSIXct(ref_aug25_start, tz = "America/Vancouver") &
                       mdot_ref$DateTime <= as.POSIXct(ref_aug25_end,   tz = "America/Vancouver")), ]

# FOCAL mooring second  ... 
x <- read_mdot( foc_cat[1] )
start <- "2025-04-01 12:00:00" # include full beginning 
end   <- "2025-06-12 12:30:00" # June pull up
mdot_foc <- x[ x$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
               x$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
#tail(mdot_foc)

# add the patch
x <- foc_patch
start <- "2025-06-12 13:30:00" # from June redeployment  
end   <- "2025-07-10 13:00:00" # to July pull up
x <- x[ x$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
        x$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
mdot_foc <- rbind( mdot_foc, x )
#tail(mdot_foc)

# add the final Cat file
x <- read_mdot( foc_cat[2] )
start <- "2025-07-10 14:15:00" # after July re-deployment 
end   <- "2025-12-12 12:00:00" # to end of timeseries 
x <- x[ x$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
        x$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
mdot_foc <- rbind( mdot_foc, x)
#tail(mdot_foc)

# Trim to deployment dates
mdot_foc <- trim_deployment( mdot_foc )

# And Trim the weird Aug blip
foc_aug25_start <- "2025-08-25 10:15:00"
foc_aug25_end   <- "2025-08-25 13:00:00"
mdot_foc <- mdot_foc[!(mdot_foc$DateTime >= as.POSIXct(foc_aug25_start, tz = "America/Vancouver") &
                       mdot_foc$DateTime <= as.POSIXct(foc_aug25_end,   tz = "America/Vancouver")), ]

## VESTIGIAL: 09/25 work on date repair ... 
#---- Show data for different pieces extracted  ---- 

# x <- read_mdot( ref_cat[1] )
# y <- ref_patch
# z <- read_mdot( ref_cat[2] )
# 
# 
# x <- mdot_ref
# start <- "2025-06-12 21:00"
# end   <- "2025-07-13 20:00"
# 
# x <- x[ x$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
#         x$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
# plot(x$Temp ~ x$DateTime, type = "l" )
# 
# 
# y <- y[ y$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
#         y$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
# z <- z[ z$DateTime >= as.POSIXct( start, tz = "America/Vancouver") &
#         z$DateTime <= as.POSIXct( end,   tz = "America/Vancouver"), ]
# 
# a <- list(
#   list(df = x,  var = "Temp", label = "Early"),
#   list(df = y,  var = "Temp", label = "Patch"),
#   list(df = z,  var = "Temp", label = "Late") )
# plot_timeseries( a, metric = "MDOT Mess")



# # Set a common temperature range
# ylim_range <- range(c(foc$Temp, ref$Temp), na.rm = TRUE)
# 
# plot(foc$Temp ~ foc$DateTime,
#      type = "l", xlab = "DateTime", ylab = "Temperature", col = "steelblue",
#      ylim = ylim_range, main = "MiniDOT Temperatures")
# par(new = TRUE)
# plot(ref$Temp ~ ref$DateTime,
#      type = "l", xlab = "", ylab = "", axes = FALSE, col = "darkorange",
#      ylim = ylim_range )
# legend("topleft",
#        legend = c("Focal", "Reference"),
#        col    = c("steelblue", "darkorange"),
#        lty    = 1, bty = "n")
# 

# For plotting, can insert gap rows to avoid graphs connecting points 
# on opposite sides of maintenance gaps

# # JUly 10 maintenance
# foc <- insert_gap( foc, "2025-07-10 23:00:00" )
# ref <- insert_gap( ref, "2025-07-10 18:00:00" )
# # Aug 25 maintenance
# foc <- insert_gap( foc, "2025-08-25 18:00:00" )
# ref <- insert_gap( ref, "2025-08-25 20:00:00" )
# 


# Fin.



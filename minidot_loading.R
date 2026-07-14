#### MINIDOT DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 30, 2025
################################################################################
# NOTES:
# Temperature is in C; DO is mg/l, and DO_sat is a %
# 2026/07/07: 2nd iteration of data loading simplified to use catenated files,
#   except see below. Fuck. 
# 2026/07/08: Data trimming cleaned up and consistent
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

# Build the MDOT dataframes ... 
mdot_ref <- read_mdot( ref_cat )
mdot_foc <- read_mdot( foc_cat )
 
mdot_ref <- rbind(ref_patch, mdot_ref[, !names(mdot_ref) %in% "DO_sat"])
mdot_foc <- rbind(foc_patch, mdot_foc[, !names(mdot_foc) %in% "DO_sat"])

mdot_ref <- mdot_ref[order(mdot_ref$DateTime), ]
mdot_foc <- mdot_foc[order(mdot_foc$DateTime), ]

# Remove duplicates 
mdot_ref <- mdot_ref[!duplicated(mdot_ref$DateTime), ]
mdot_foc <- mdot_foc[!duplicated(mdot_foc$DateTime), ]

# Trim to deployment dates
mdot_foc <- trim_deployment( mdot_foc )
mdot_ref <- trim_deployment( mdot_ref )

# Removed manually identified maintenance periods
mdot_foc <- trim_foc_maintenance( mdot_foc )
mdot_ref <- trim_ref_maintenance( mdot_ref )

#---- Show data for selected date range ---- 

# optionally can trim to test dates
#sdate <- "2025-08-25 15:00:00"
#edate <- "2025-08-265 00:00:00"
#foc <- trim_deployment( mdot_foc )
#ref <- trim_deployment( mdot_ref )

foc <- mdot_foc
ref <- mdot_ref

# Plot raw temperature data from both sensors 

# Insert gap rows to avoid graphs connecting points on opposite sides of the gaps
# JUly 10 maintenance
foc <- insert_gap( foc, "2025-07-10 23:00:00" )
ref <- insert_gap( ref, "2025-07-10 18:00:00" )
# Aug 25 maintenance
foc <- insert_gap( foc, "2025-08-25 18:00:00" )
ref <- insert_gap( ref, "2025-08-25 20:00:00" )


# Set a common temperature range
ylim_range <- range(c(foc$Temp, ref$Temp), na.rm = TRUE)

plot(foc$Temp ~ foc$DateTime,
     type = "l", xlab = "DateTime", ylab = "Temperature", col = "steelblue",
     ylim = ylim_range, main = "MiniDOT Temperatures")
par(new = TRUE)
plot(ref$Temp ~ ref$DateTime,
     type = "l", xlab = "", ylab = "", axes = FALSE, col = "darkorange", 
     ylim = ylim_range )
legend("topleft",
       legend = c("Focal", "Reference"),
       col    = c("steelblue", "darkorange"),
       lty    = 1, bty = "n")


# Fin.



#### DST DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 30, 2025
############################################################################----

# DST collected via the Star-ODDI sensor includes depth, temperature, salinity, and conductivity
# Four Star-Oddis were deployed in total, a primary in a cage, and a secondary without 
# Focal 1 = S12074, Focal 2 = S12666
# Ref 1   = S12665; Ref 2  =  S12668 

#------ Data loading section ----
# After trying to load xlsx files in a few ways, settled on using preprocessed, Matlab
# .txt files created by Wiley for his preliminary plots.

# Location of sensor folders ... 
oddi_dir <- paste0( source_dir, '/DST' )

# The individual sensor data files ... 
ffocal1 <- '/S12074/kelp_DST_matlab_test.txt'
ffocal2 <- '/S12666/kelp_DST_matlab.txt'

fref1   <- '/S12665/ref_DST_matlab_test.txt'
#fref2   <- '/S12665/ref_DST_matlab.txt'     
# not in the expected subdir or format. used xls files directly.

clean_names <- c( "excel_date", "Temp", "Depth", "Salinity", "Conductivity", "Sound_Velocity")

# Build the focal site dataframe ... 
focal_DST1 <- read.delim( paste0(oddi_dir, ffocal1) )
focal_DST2 <- read.delim( paste0(oddi_dir, ffocal2) )

# tidy column names ... 
names(focal_DST1) <- clean_names
names(focal_DST2) <- clean_names
# add date/time stamp
focal_DST1$DateTime <- as.POSIXct( (focal_DST1$excel_date - 25569) * 86400,
                                    origin = "1970-01-01", tz = "UTC" )
focal_DST2$DateTime <- as.POSIXct( (focal_DST2$excel_date - 25569) * 86400,
                                  origin = "1970-01-01", tz = "UTC" )

# Build the reference site dataframe ... 
ref_DST1 <- read.delim( paste0(oddi_dir, fref1) )
ref_DST2 <- rbind( read_xls( paste0( oddi_dir, '/S12665/6S12665.xls' )),
                   read_xls( paste0( oddi_dir, '/S12665/7S12665.xls' )),
                   read_xls( paste0( oddi_dir, '/S12665/8S12665.xls' )),
                   read_xls( paste0( oddi_dir, '/S12665/9S12665.xls' )) )

# tidy column names ... 
names(ref_DST1) <- clean_names
names(ref_DST2) <- clean_names

# add date/time stamp
ref_DST1$DateTime <- as.POSIXct( (ref_DST1$excel_date - 25569) * 86400,
                                  origin = "1970-01-01", tz = "UTC" )
ref_DST2$DateTime <- as.POSIXct( (ref_DST2$excel_date - 25569) * 86400,
                                 origin = "1970-01-01", tz = "UTC" )

# Check an single data set ... 
ggplot(x, aes(x = DateTime)) +
  geom_line(aes(y = Temp)) +
  labs(
    x = "Date/Time",
    y = "Temperature (°C)",
    title = "Temperature Over Time"
  ) +
  theme_bw()

#---- Data cleaning ---- 

# Apply (some of) Wiley's windows from the MatLab script
#==> NOTE that the moorings would ideally have their own windows.
trim_win <- data.frame(
  start = c( 739780.71140319, 739780.900314626, 739780.900277715, 739808.713883097,
             739854.813608523,739854.828527756, 739876.703932703,  739876.958356482 ),
  end = c( 739780.733188733, 739780.907080802, 739780.936459367, 739808.769629938,
           739854.837604885, 739854.887540078, 739876.969823448, 0 )
)

intervals_to_posix( trim_win )



# Trim the DST data to the above windows
tfocal_DST1 <- trim_by_matlab_windows( focal_DST1, trim_win )
tfocal_DST2 <- trim_by_matlab_windows( focal_DST2, trim_win )
tref_DST1   <- trim_by_matlab_windows( ref_DST2, trim_win )
tref_DST2   <- trim_by_matlab_windows( ref_DST2, trim_win )


#---- Data visualization ---- 

#---- Show all data ----
ggplot() +
  geom_line(data = tfocal_DST1, aes(x = DateTime, y = Temp, color = "F1"),   linewidth = 0.7) +
  geom_line(data = tfocal_DST2, aes(x = DateTime, y = Temp, color = "F2"),  linewidth = 0.7) +
  geom_line(data = tref_DST1,   aes(x = DateTime, y = Temp, color = "R1"), linewidth = 0.7) +
  geom_line(data = tref_DST2,   aes(x = DateTime, y = Temp, color = "R2"), linewidth = 0.7) +
  scale_color_manual(values = c("F1" = "red",
                                "F2" = "blue",
                                "R1" = "black",
                                "R2" = "green")) +
  labs(
    x = "Time",
    y = "Temperature (°C)",
    title = "Temperature Time Series",
    color = "Sensor"
  ) +
  theme_bw() +
  theme(legend.position = "bottom")

###---> Stop here. 




#-------------- Functions -------------


# Trim rows between one or more MATLAB-datenum intervals (inclusive)
# df must have a POSIXct column named DateTime (ChatGPT)
trim_by_matlab_windows <- function(df, intervals, tz = "UTC") {
  stopifnot("DateTime" %in% names(df))
  # ensure POSIXct and consistent tz
  df$DateTime <- as.POSIXct(df$DateTime, tz = tz)
  
  # helper: MATLAB datenum -> POSIXct
  m2t <- function(x) as.POSIXct((x - 719529) * 86400,
                                origin = "1970-01-01", tz = tz)
  
  # data bounds
  tmin <- min(df$DateTime, na.rm = TRUE)
  tmax <- max(df$DateTime, na.rm = TRUE)
  
  # expect a data.frame with columns 'start' and 'end' (numeric MATLAB datenums, 0 allowed)
  keep <- rep(TRUE, nrow(df))
  for (i in seq_len(nrow(intervals))) {
    s_raw <- intervals$start[i]
    e_raw <- intervals$end[i]
    s <- if (s_raw == 0) tmin else m2t(s_raw)
    e <- if (e_raw == 0) tmax else m2t(e_raw)
    keep <- keep & !(df$DateTime >= s & df$DateTime <= e)
  }
  df[keep, , drop = FALSE]
}

# This displays the intervals defined using MatLab time intervals to readable Posix
intervals_to_posix <- function(df, intervals, tz = "UTC") {
  stopifnot("DateTime" %in% names(df))
  
  # Ensure DateTime is POSIXct
  df$DateTime <- as.POSIXct(df$DateTime, tz = tz)
  
  # Helper to convert MATLAB datenum to POSIXct
  m2t <- function(x) as.POSIXct((x - 719529) * 86400,
                                origin = "1970-01-01", tz = tz)
  
  # Bounds of actual data
  tmin <- min(df$DateTime, na.rm = TRUE)
  tmax <- max(df$DateTime, na.rm = TRUE)
  
  # Convert the intervals
  out <- intervals
  out$start_posix <- ifelse(out$start == 0, tmin, m2t(out$start))
  out$end_posix   <- ifelse(out$end   == 0, tmax, m2t(out$end))
  
  out
}



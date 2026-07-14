############################################################################----
# DST (Star-Oddi) DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# NOTES:
# 2026/07/08: Data loading review and cleaned up
#             Data trimming applied consistently
############################################################################

# DST collected via the Star-ODDI sensor includes depth, temperature, salinity, and conductivity
# Four Star-Oddis were deployed in total, a primary in a cage, and a secondary without 
# Focal 1 = S12074, Focal 2 = S12666
# Ref 1   = S12665; Ref 2   = S12668 (Ref 2 added during July 10 maintenance)

#------ Data loading section ----
# After trying to load xlsx files in a few ways, settled on using preprocessed, Matlab
# .txt files created by Wiley for his preliminary plots.

# Location of sensor folders ... 
oddi_dir <- paste0( source_dir, '/DST' )

# The individual sensor data files ... 
DST_focal1_nm <- '/S12074/kelp_DST_matlab_test.txt'
DST_focal2_nm <- '/S12666/kelp_DST_matlab.txt'

DST_ref1_nm <- '/S12665/ref_DST_matlab_test.txt'
DST_ref2_nm <- '/S12668/ref_DST_matlab.txt'     

clean_names <- c( "excel_date", "Temp", "Depth", "Salinity", "Conductivity", "Sound_Velocity")

# Create initial data frames from Wiley's text files ....
# fileEncoding required to deal with funky characters in the headings.
DST_foc1 <- read.delim( paste0(oddi_dir, DST_focal1_nm), fileEncoding = "latin1" )
DST_foc2 <- read.delim( paste0(oddi_dir, DST_focal2_nm), fileEncoding = "latin1" )
DST_ref1 <- read.delim( paste0(oddi_dir, DST_ref1_nm), fileEncoding = "latin1" )
DST_ref2 <- read.delim( paste0(oddi_dir, DST_ref2_nm), fileEncoding = "latin1" )

# tidy column names ... 
names(DST_foc1) <- clean_names
names(DST_foc2) <- clean_names
names(DST_ref1) <- clean_names
names(DST_ref2) <- clean_names

# add date/time stamps
DST_foc1$DateTime <- as.POSIXct( (DST_foc1$excel_date - 25569) * 86400,
                                    origin = "1970-01-01", tz = "UTC" )
DST_foc2$DateTime <- as.POSIXct( (DST_foc2$excel_date - 25569) * 86400,
                                  origin = "1970-01-01", tz = "UTC" )
DST_ref1$DateTime <- as.POSIXct( (DST_ref1$excel_date - 25569) * 86400,
                                   origin = "1970-01-01", tz = "UTC" )
DST_ref2$DateTime <- as.POSIXct( (DST_ref2$excel_date - 25569) * 86400,
                                   origin = "1970-01-01", tz = "UTC" )

# Check date ranges
range(DST_foc1$DateTime)
range(DST_foc2$DateTime)
range(DST_ref1$DateTime)
range(DST_ref2$DateTime)

# Trim to deployment range
DST_foc1 <- trim_deployment( DST_foc1 )
DST_foc2 <- trim_deployment( DST_foc2 )
DST_ref1 <- trim_deployment( DST_ref1 )
DST_ref2 <- trim_deployment( DST_ref2 )

# Trim to maintenance windows
DST_foc1 <-trim_foc_maintenance( DST_foc1 )
DST_foc2 <-trim_foc_maintenance( DST_foc2 )
DST_ref1 <-trim_ref_maintenance( DST_ref1 )
DST_ref2 <-trim_ref_maintenance( DST_ref2 )


#---- Data visualization ---- 

#---- Show all data ----
ggplot() +
  geom_line(data = DST_foc1, aes(x = DateTime, y = Temp, color = "F1"), linewidth = 0.7) +
  geom_line(data = DST_foc2, aes(x = DateTime, y = Temp, color = "F2"), linewidth = 0.7) +
  geom_line(data = DST_ref1, aes(x = DateTime, y = Temp, color = "R1"), linewidth = 0.7) +
  geom_line(data = DST_ref2, aes(x = DateTime, y = Temp, color = "R2"), linewidth = 0.7) +
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

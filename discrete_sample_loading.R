#===============================================================================
# Script:  discrete_sample_loading.R
# Purpose: Load and visualise DST (Star-Oddi) data provided by Wiley
# Created: Sept 2025
# NOTES: 
# Significant bits of this code, esp. string processing) provided by ChatGPT. 
#===============================================================================
# Updates:
# 2026/10/05: Visited again
# 2025/10/28: Updated for consistency 
# 2026/10: Integrated with the LSSM_Water_Analysis project
# 2026/11/05: Corrected time stamp
#===============================================================================

#---- Part 1: Load and format the excel data for analysis. -------------------------------

# Load early version of discrete sample data. Allow read_excel() to guess types. Does a good job so far. 
#raw_data <- read_excel( paste0( source_dir, '/Discrete_sample_data/BKP_Metadata_EdModified_ver2025Dec26.xls' ))
# Load processed version of discrete sample data.
raw_data <- read_excel( paste0( source_dir, '/Discrete_sample_data/discrete_data_for_model_development.xls' ))

# see raw data column names 
names( raw_data )

# Pull and simplify names of a relevant subset of data
clean_data <- raw_data[, c("Station_ID", "Collection_Date", "Collection_Time_PST",
                           "NIST_Temp", "YSI_S", "pCO2@insituT (uatm)", "pH (total)", "Omega_Arag")]
colnames(clean_data) <- c("Station", "Date", "Time", "Temp", "Salinity", "pCO2", "pH", "OArag")

#--- 
# SOME DATA CLEAN UP required at this point to either drop samples, or 
# interpolate sample times, correct labels or other things. 
# Preferable to do here or in the raw data file? 
#---  

# Drop 9 samples where collection time or temperature not recorded on log sheets
clean_data <- clean_data[ !clean_data$Time =="NA", ]

# Create POSIX column from date and add time. 
clean_data$DateTime <- as.POSIXct(format(clean_data$Date, "%Y-%m-%d", tz = "UTC"),
                                  tz = "America/Vancouver") +
                       round(as.numeric(clean_data$Time) * 86400)

# Now drop the individual Date and Time columns ... 
clean_data <- clean_data[, !names(clean_data) %in% c("Date", "Time")]

# Standardize station names: 
#   Replace 'in' with '_In' and 'out' with '_Out' at the end of station names
clean_data$Station <- gsub("\\s*[iI][nN]$", "_In", clean_data$Station)
clean_data$Station <- gsub("\\s*[oO][uU][tT]$", "_Out", clean_data$Station)

# Temp to number, and then the whole thing as a dataframe  
clean_data$Temp <- as.numeric( clean_data$Temp )
disc_data <- as.data.frame(clean_data)
rm( clean_data )
# Check dataframe
head(disc_data)


#---- Fin. ----
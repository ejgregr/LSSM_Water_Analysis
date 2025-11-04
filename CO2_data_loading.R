#### CO2 Pro  DATA LOADING and VISUALIZATION ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 30, 2025
############################################################################----


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
full_plot( plot_dat )

# Remove the offending bit of the time series
ref_dat <- ref_dat[ ref_dat$Timestamp > as.POSIXct("2025-07-15", tz = "UTC"), ]

# Plot it again.
plot_dat <- rbind(focal_dat, ref_dat)
full_plot( plot_dat )

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
diff_dat <- merge(
  focal_dat[, c("Timestamp", "CO2")],
  ref_dat[,   c("Timestamp", "CO2")],
  by = "Timestamp",
  suffixes = c("_focal", "_ref")
)

# now add the difference column and plot it
diff_dat$CO2_diff <- diff_dat$CO2_focal - diff_dat$CO2_ref
plot_diff( diff_dat )

###---> Stop cuz somehow (likely bc of the merge) there are duplicates in diff_dat.

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




#-------------- Functions -------------
# This function loads and combines the 4 files from each mooring to create 
# the full CO2 time series. There is one annoyance in that the reference site data
# has one file that is tab delimited, while the other 7 files are comma delimited. 
# This requires the condiional, delimiter-based read in the function.  
load_and_bind <- function(files, cols, tz = "UTC") {
  out <- do.call(rbind, lapply(files, function(f) {
    
    # --- Detect delimiter from first line ---
    first_line <- readLines(f, n = 1)
    is_comma <- grepl(",", first_line)
    
    # --- Read file accordingly ---
    if (is_comma) {
      df <- read.csv(f, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    } else {
      df <- read.delim(f, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
    }
    
    df <- df[-1, , drop = FALSE]                    # drop the formatting row
    df[] <- lapply(df, type.convert, as.is = TRUE)  # restore numeric types
    df <- df[, cols, drop = FALSE]                  # keep only needed columns
    df$Timestamp <- as.POSIXct(sprintf(
      "%04d-%02d-%02d %02d:%02d:%02d",
      df$Year, df$Month, df$Day, df$Hour, df$Minute, df$Second
    ), tz = tz)
    df
  }))
  rownames(out) <- NULL
  out
}

full_plot <-function( plot_dat ){
  ggplot(plot_dat, aes(x = Timestamp, y = CO2, color = Dataset)) +
    geom_line(alpha = 0.8) +
    labs(
      x = "Time",
      y = "CO2 (ppm or mg/m³ — whichever applies)",
      color = "Dataset",
      title = "CO2 Time Series Comparison"
    ) +
    theme_bw()
}  

plot_diff <-function( plot_dat ){
  ggplot(plot_dat, aes(x = Timestamp, y = CO2_diff)) +
    geom_line(color = "black") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      x = "Time",
      y = "CO₂ Difference (Focal − Reference)",
      title = "CO₂ Difference Over Time"
    ) +
    theme_bw()
}

# Reduces temporal resolution to hourly, and computes mean and sd by hour
hourly_stats <- function(df) {
  # Ensure Timestamp is POSIXct
  df$Timestamp <- as.POSIXct(df$Timestamp)
  
  # Create an hourly timestamp (truncate to hour)
  df$Hour <- as.POSIXct(format(df$Timestamp, "%Y-%m-%d %H:00:00"), tz = attr(df$Timestamp, "tzone"))
  
  # Compute mean and sd by hour
  agg_mean <- aggregate(CO2 ~ Hour, df, mean)
  agg_sd   <- aggregate(CO2 ~ Hour, df, sd)
  
  # Merge results
  result <- merge(agg_mean, agg_sd, by = "Hour", suffixes = c("_mean", "_sd"))
  
  result
}

# As above but daily.
daily_stats <- function(df) {
  # Ensure Timestamp is POSIXct (no harm if it already is)
  df$Timestamp <- as.POSIXct(df$Timestamp)
  
  # Make a daily date column
  df$Date <- as.Date(df$Timestamp)
  
  # Mean per day
  mean_daily <- aggregate(CO2 ~ Date, df, mean)
  
  # SD per day (use 0 if only one sample that day)
  sd_daily <- aggregate(CO2 ~ Date, df, function(x) if (length(x) > 1) sd(x) else 0)
  
  # Merge results
  result <- merge(mean_daily, sd_daily, by = "Date", suffixes = c("_mean", "_sd"))
  
  return(result)
}

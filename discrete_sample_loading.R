#### DISCRETE SAMPLE LOADING ###
# Significant bits of this code, particularly string processing, were provided by ChatGPT. 
# Updated: Oct 20, 2025
############################################################################----

# Load early version of discrete sample data. Allow read_excel() to guess types. Does a good job so far. 
raw_data <- read_excel( paste0( source_dir, '/BKP_Metadata_EdModified_ver2025Aug22.xls' ))
# see raw data column names 
names( raw_data)

# Pull and simplify names of a relevant subset of data
clean_data <- raw_data[, c("Station_ID", "Collection_Date", "Collection_Time_PST",
                           "NIST_Temp", "YSI_S", "pCO2@insituT (uatm)", "pH (total)")]
colnames(clean_data) <- c("Station", "Date", "Time", "Temp", "Salt", "pCO2", "pH")

#--- 
# SOME DATA CLEAN UP will be required at this point to either drop samples, or 
# interpolate sample times, correct labels or other things. 
# Preferable to do here or in the raw data file? 
#---  

# Adjust and format Date and Time columns
# Convert Excel fractional day value -> seconds -> 24 hr time
clean_data$Time <- hms::hms(round(as.numeric(clean_data$Time) * 86400))
# Format date
clean_data$Date <- as.Date(clean_data$Date)
# Add DateTime column
clean_data$DateTime <- as.POSIXct(clean_data$Date) + clean_data$Time


# Standardize station names 
#   Replace 'in' with '_In' and 'out' with '_Out' at the end of station names
clean_data$Station <- gsub("\\s*[iI][nN]$", "_In", clean_data$Station)
clean_data$Station <- gsub("\\s*[oO][uU][tT]$", "_Out", clean_data$Station)

# Check dataframe
head(clean_data)
class( clean_data$DateTime )

#---
# Partition the data for analysis. We start to lose records here ... 
#---

# 1) Parse site and in/out flag from Station
dat <- clean_data %>%
  mutate(
    Site = str_remove(Station, "_(In|Out)$"),
    IO   = case_when(
      str_detect(Station, "_In$")  ~ "In",
      str_detect(Station, "_Out$") ~ "Out",
      TRUE ~ NA_character_
    )
  ) %>%
  # Keep only rows with valid suffix
  filter(!is.na(IO))

# 2) Split into In and Out tables
in_tbl <- dat %>%
  filter(IO == "In") %>%
  select(Site, Date, DateTime_in = DateTime, Time_in = Time, Temp_in = Temp, pH_in = pH)

out_tbl <- dat %>%
  filter(IO == "Out") %>%
  select(Site, Date, DateTime_out = DateTime, Time_out = Time, Temp_out = Temp, pH_out = pH)

in_tbl
out_tbl

# Confirm time types are valid ... 
#sapply(list(in_tbl$DateTime_in, out_tbl$DateTime_out), class)

# 1) Equality join on Site + Date to get candidate pairs
candidates <- inner_join(
  out_tbl, in_tbl,
  by = c("Site", "Date"),
  suffix = c("_out", "_in")
)

# 2) Compute time gap (mins), keep nearest In for each Out, and filter to ≤ 60 min
ph_diff <- candidates %>%
  mutate(dt_gap = abs(as.numeric(difftime(DateTime_out, DateTime_in, units = "mins")))) %>%
  group_by(Site, Date, DateTime_out) %>%
  slice_min(dt_gap, with_ties = FALSE) %>%
  ungroup() %>%
  filter(dt_gap <= 60) %>%
  mutate(
    d_pH = pH_out - pH_in,
    DateTime_mid = as_datetime((as.numeric(DateTime_out) + as.numeric(DateTime_in)) / 2)
  ) %>%
  select(
    Site, Date,
    DateTime_in,  Time_in,  Temp_in,  pH_in,
    DateTime_out, Time_out, Temp_out, pH_out,
    dt_gap, DateTime_mid, d_pH
  )

# Peek
head(ph_diff)


# Ensure we only plot good pairs
plot_df <- ph_diff %>%
  filter(!is.na(d_pH)) %>%
  mutate(
    # Make sure it's POSIXct and set TZ for nice axis labels
    DateTime_mid = as.POSIXct(DateTime_mid, tz = "America/Vancouver")
  )

p <- ggplot(plot_df, aes(x = DateTime_mid, y = d_pH, group = Site)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_line() +
  geom_point(size = 0.9) +
  facet_wrap(~ Site, ncol = 3, scales = "free_x") +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  labs(
    title = "pH Difference by Site (Out-In)",
    x = "Date",
    y = expression(Delta*" pH")
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_rect(fill = "grey90", color = NA),
    plot.title = element_text(face = "bold")
  )
p

# Facet plot of reference sites over time ... 
# Keep only Sites that start with "R"
# Filter only R sites (R1, R2, ... R11 etc.)
dat_R <- clean_data %>%
  filter(grepl("^R[0-9]+$", Station)) %>% 
  mutate(
    Station = factor(Station, levels = paste0("R", 1:11)),  # order the facets
    DateTime = as.POSIXct(DateTime, tz = "America/Vancouver"),
  )


# Plot pH over time, faceted by R station
p <- ggplot(dat_R, aes(x = DateTime, y = pH, group = Station)) +
  geom_line() +
  geom_point(size = 0.9) +
  # add scales = "free_y" to facet_wrap() to allow individual y axes.
  facet_wrap(~ Station, ncol = 3) +
  scale_x_datetime(date_breaks = "1 week", date_labels = "%b %d") +
  
  labs(
    title = "pH over time at R sites",
    x = "Date",
    y = "pH (total)"
  ) +
  theme_bw(base_size = 11) +
  theme(
     panel.grid.minor = element_blank(),
     axis.text.x = element_text(angle = 45, hjust = 1),
     strip.background = element_rect(fill = "grey90", color = NA),
     plot.title = element_text(face = "bold")
   )
p

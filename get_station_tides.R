#===============================================================================
# Script:  get_station_tides.R
# Purpose: Query the DFO/CHS Integrated Water Level System (IWLS) REST API
#          for tidal predictions a selected station ,
#          the reference station for the Broughton Archipelago.
# API docs: https://api-iwls.dfo-mpo.gc.ca/swagger-ui.html
# No authentication required.
#===============================================================================
# NOTES:
#  A SINGLE USE script to pull data from DFO and save tide heights to CSV.
#  Loading CSV and working with tide heights limited to data_validation.R 
#===============================================================================
library(httr)

#----------------------------------------------------------------------------
# Configuration
#----------------------------------------------------------------------------
BASE_URL    <- "https://api-iwls.dfo-mpo.gc.ca/api/v1"

#stn_ids:
# NAME        STATION       DOF UIID
# Alert Bay   08280         5cebf1df3d0f4a073c4bbbb8


STATION_ID  <- "5cebf1df3d0f4a073c4bbbb8"   # Alert Bay internal UUID
                                              # (retrieved via station code 08280)

# Time window: adjust to match your field deployment
# API requires ISO 8601 UTC format
time_start  <- "2025-04-24T00:00:00Z"
time_end    <- "2025-09-30T23:59:59Z"

# Time series codes:
#   "wlp"      = tidal predictions (hourly)
#   "wlp-hilo" = predicted high/low water only
#   "wlo"      = observed water levels (where available)
SERIES_CODE <- "wlp"

#----------------------------------------------------------------------------
# Helper: GET request with error handling
#----------------------------------------------------------------------------
chs_get <- function(endpoint, query = list()) {
  url <- paste0(BASE_URL, endpoint)
  resp <- GET(url, query = query)
  if (http_error(resp)) {
    stop(sprintf("API request failed [%s]: %s",
                 status_code(resp), url))
  }
  content(resp, as = "parsed", type = "application/json")
}

#----------------------------------------------------------------------------
# Step 1: Confirm station metadata
#----------------------------------------------------------------------------
cat("Fetching station metadata...\n")
meta <- chs_get(paste0("/stations/", STATION_ID))
cat(sprintf("Station: %s (%s)\n", meta$officialName, meta$code))
cat(sprintf("Location: %.4f, %.4f\n", meta$latitude, meta$longitude))
cat(sprintf("Time zone: %s\n\n", meta$timeZoneCode))

#----------------------------------------------------------------------------
# Step 2: Query tidal predictions
# Note: API limits requests to 30 days per call; loop over months if needed
#----------------------------------------------------------------------------
cat("Fetching tidal predictions...\n")

# Split date range into monthly chunks to respect API limits
start_dt <- as.POSIXct(time_start, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
end_dt   <- as.POSIXct(time_end,   format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# Generate monthly sequence of start dates
month_starts <- seq(start_dt, end_dt, by = "month")

tide_list <- vector("list", length(month_starts))

for (i in seq_along(month_starts)) {
  chunk_start <- month_starts[i]
  chunk_end   <- min(chunk_start + 30 * 24 * 3600 - 1, end_dt)

  fmt_start <- format(chunk_start, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  fmt_end   <- format(chunk_end,   "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  cat(sprintf("  Requesting %s to %s\n", fmt_start, fmt_end))

  result <- chs_get(
    endpoint = paste0("/stations/", STATION_ID, "/data"),
    query    = list(
      time_series_code = SERIES_CODE,
      from             = fmt_start,
      to               = fmt_end
    )
  )

  if (length(result) > 0) {
    tide_list[[i]] <- do.call(rbind, lapply(result, function(x) {
      data.frame(
        DateTime_UTC = as.POSIXct(x$eventDate,
                                  format = "%Y-%m-%dT%H:%M:%SZ",
                                  tz = "UTC"),
        WaterLevel_m = as.numeric(x$value),
        qcFlag       = x$qcFlagCode,
        stringsAsFactors = FALSE
      )
    }))
  }

  Sys.sleep(0.5)  # be polite to the API
}

#----------------------------------------------------------------------------
# Step 3: Combine and convert to local time (Pacific)
#----------------------------------------------------------------------------
tides <- do.call(rbind, tide_list)
tides$DateTime_PT <- format(tides$DateTime_UTC,
                             tz = "America/Vancouver",
                             usetz = TRUE)

cat(sprintf("\nRetrieved %d records\n", nrow(tides)))
cat(sprintf("Date range: %s to %s\n",
            min(tides$DateTime_UTC), max(tides$DateTime_UTC)))
cat(sprintf("Water level range: %.2f to %.2f m\n",
            min(tides$WaterLevel_m), max(tides$WaterLevel_m)))

#----------------------------------------------------------------------------
# Step 4: Quick plot
#----------------------------------------------------------------------------
plot(tides$DateTime_UTC, tides$WaterLevel_m,
     type = "l",
     xlab = "Date (UTC)",
     ylab = "Predicted Water Level (m, Chart Datum)",
     main = "Alert Bay Tidal Predictions — IWLS API")

#----------------------------------------------------------------------------
# Step 5: Save
#----------------------------------------------------------------------------
saveRDS(tides, "tides_alertbay.rds")
write.csv(tides, "tides_alertbay.csv", row.names = FALSE)
cat("\nSaved: tides_alertbay.rds and tides_alertbay.csv\n")

### Fin.

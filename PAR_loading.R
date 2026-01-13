library(ggplot2)

# Directory containing monthly CSVs
csv_dir <- "GEE_exports"

# Read and combine
files <- list.files( path = paste0(source_dir, "/GEE_exports"),
                             pattern = "\\.csv$",
                             full.names = TRUE )


era5_df <- do.call(
  rbind,
  lapply(files, function(f) {
    d <- read.csv(f, stringsAsFactors = FALSE)
    d$Timestamp <- as.POSIXct(d$Timestamp, tz = "UTC")
    d
  })
)

# Keep only required columns
era5_df <- era5_df[, c("Timestamp", "mean", "stdDev")]

# Ensure time ordering
era5_df <- era5_df[order(era5_df$Timestamp), ]

head(era5_df)


library(ggplot2)

ggplot(era5_df, aes(x = Timestamp, y = mean)) +
  geom_errorbar(
    aes(
      ymin = mean - stdDev,
      ymax = mean + stdDev
    ),
    width = 0,            # vertical lines only
    alpha = 0.5,
    linewidth = 0.4
  ) +
  geom_point(
    size = 1.2,
    color = "black"
  ) +
  labs(
    x = "Time (UTC)",
    y = expression("Surface solar radiation downwards (J m"^-2~")"),
    title = "ERA5 hourly surface solar radiation (mean ± SD)"
  ) +
  theme_bw()


# downsample to daily ... 

era5_daily <- aggregate(
  cbind(mean, stdDev) ~ as.Date(Timestamp),
  data = era5_df,
  FUN = mean
)

names(era5_daily)[1] <- "Date"

ggplot(era5_daily, aes(x = Date, y = mean)) +
  geom_errorbar(
    aes(
      ymin = mean - stdDev,
      ymax = mean + stdDev
    ),
    width = 0,            # vertical lines only
    alpha = 0.5,
    linewidth = 0.4
  ) +
  geom_point(
    size = 1.2,
    color = "black"
  ) +
  labs(
    x = "Date",
    y = expression("Surface solar radiation downwards (J m"^-2~")"),
    title = "ERA5 daily surface solar radiation (mean ± SD)"
  ) +
  theme_bw()

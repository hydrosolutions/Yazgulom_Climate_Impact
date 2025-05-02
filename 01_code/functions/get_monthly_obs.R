# Function to prepare monthly observations
get_monthly_obs <- function() {
  # Create data frame with dates and observed values
  df <- data.frame(
    date = basin_obs_ts$date,
    Q = basin_obs_ts$Q_mm_day_no_glacier
  )
  
  # Add month column and aggregate
  df$month <- format(df$date, "%Y-%m")
  monthly_data <- aggregate(Q ~ month, data = df, FUN = mean, na.rm = TRUE)
  
  # Extract observations for calibration period
  monthly_calib <- monthly_data$Q[
    which(as.Date(paste0(monthly_data$month, "-01")) >= cal_val_per$calib_start &
            as.Date(paste0(monthly_data$month, "-01")) <= cal_val_per$calib_end)
  ]
  
  return(monthly_calib)
}
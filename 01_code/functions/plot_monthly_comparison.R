# Plot function for monthly comparison
plot_monthly_comparison <- function(param, period_indices, title = "Monthly Comparison") {
  # Run model with parameters
  runResults <- run_model_with_params(param)
  
  # Get dates
  sim_dates <- basin_obs_ts$date[period_indices]
  
  # Create dataframe for simulated values
  df_sim <- data.frame(
    date = sim_dates,
    Qsim = runResults$Qsim
  )
  
  # Add month column and aggregate
  df_sim$month <- format(df_sim$date, "%Y-%m")
  sim_monthly <- aggregate(Qsim ~ month, data = df_sim, FUN = mean, na.rm = TRUE)
  
  # Create dataframe for observed values
  df_obs <- data.frame(
    date = basin_obs_ts$date,
    Qobs = basin_obs_ts$Q_mm_day_no_glacier
  )
  
  # Filter to relevant period
  df_obs <- df_obs[df_obs$date %in% sim_dates, ]
  
  # Add month column and aggregate
  df_obs$month <- format(df_obs$date, "%Y-%m")
  obs_monthly <- aggregate(Qobs ~ month, data = df_obs, FUN = mean, na.rm = TRUE)
  
  # Merge the data
  monthly_data <- merge(sim_monthly, obs_monthly, by = "month", all = TRUE)
  
  # Convert month column to proper date
  monthly_data$date <- as.Date(paste0(monthly_data$month, "-01"))
  
  # Sort by date
  monthly_data <- monthly_data[order(monthly_data$date), ]
  
  # Calculate NSE for display
  n_common <- sum(!is.na(monthly_data$Qsim) & !is.na(monthly_data$Qobs))
  if (n_common > 0) {
    valid_rows <- !is.na(monthly_data$Qsim) & !is.na(monthly_data$Qobs)
    nse <- 1 - sum((monthly_data$Qsim[valid_rows] - monthly_data$Qobs[valid_rows])^2) / 
      sum((monthly_data$Qobs[valid_rows] - mean(monthly_data$Qobs[valid_rows]))^2)
    title_with_nse <- paste0(title, " (NSE = ", round(nse, 3), ")")
  } else {
    title_with_nse <- title
  }
  
  # Create plot using base R
  plot(monthly_data$date, monthly_data$Qsim, type = "o", col = "blue",
       xlab = "Date", ylab = "Discharge (mm/day)",
       main = title_with_nse, ylim = range(c(monthly_data$Qsim, monthly_data$Qobs), na.rm = TRUE))
  points(monthly_data$date, monthly_data$Qobs, type = "o", col = "red")
  legend("topright", legend = c("Simulated", "Observed"),
         col = c("blue", "red"), lty = 1, pch = 1)
  
  # Return the data for further analysis
  return(invisible(monthly_data))
}
# Custom NSE function for monthly comparison
calculate_monthly_nse <- function(obs_monthly, sim_daily, sim_dates) {
  # Create data frame with simulated values
  df_sim <- data.frame(
    date = sim_dates,
    Qsim = sim_daily
  )
  
  # Add month column and aggregate
  df_sim$month <- format(df_sim$date, "%Y-%m")
  monthly_sim <- aggregate(Qsim ~ month, data = df_sim, FUN = mean, na.rm = TRUE)
  
  # Ensure we compare the same number of months
  n_months <- min(length(obs_monthly), nrow(monthly_sim))
  
  if(n_months < 1) {
    return(NA) # Not enough data
  }
  
  # Get values to compare
  sim_values <- monthly_sim$Qsim[1:n_months]
  obs_values <- obs_monthly[1:n_months]
  
  # Calculate NSE
  mean_obs <- mean(obs_values, na.rm = TRUE)
  nse <- 1 - sum((sim_values - obs_values)^2, na.rm = TRUE) / 
    sum((obs_values - mean_obs)^2, na.rm = TRUE)
  
  return(nse)
}
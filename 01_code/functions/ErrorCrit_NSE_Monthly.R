# Fixed monthly NSE criterion function for airGR
ErrorCrit_NSE_Monthly <- function(InputsCrit, OutputsModel, warnings = TRUE, ...) {
  # This function structure is needed for airGR compatibility
  
  # Define the actual calculation function
  .FUN <- function(Obs, Sim, ...) {
    # Try-catch to handle potential errors
    tryCatch({
      # Get dates from InputsCrit
      dates <- InputsCrit$InputsModel$DatesR[InputsCrit$RunOptions$IndPeriod_Run]
      
      # Create dataframe for monthly aggregation of simulated values
      df_sim <- data.frame(
        date = dates,
        Qsim = Sim
      )
      
      # Add month identifier (YYYY-MM format)
      df_sim$month <- format(df_sim$date, "%Y-%m")
      
      # Aggregate to monthly means
      monthly_sim <- aggregate(Qsim ~ month, data = df_sim, FUN = mean, na.rm = TRUE)
      
      # Get monthly observations (already aggregated)
      monthly_obs <- Obs
      
      # Ensure we compare the same number of months
      n_months <- min(length(monthly_obs), nrow(monthly_sim))
      
      if (n_months < 2) {
        if (warnings) warning("Not enough months to calculate NSE")
        return(NA) # Not enough data
      }
      
      # Get values to compare
      sim_values <- monthly_sim$Qsim[1:n_months]
      obs_values <- monthly_obs[1:n_months]
      
      # Calculate NSE
      mean_obs <- mean(obs_values, na.rm = TRUE)
      denominator <- sum((obs_values - mean_obs)^2, na.rm = TRUE)
      
      if (denominator > 0) {
        nse <- 1 - sum((sim_values - obs_values)^2, na.rm = TRUE) / denominator
      } else {
        if (warnings) warning("Denominator is zero in NSE calculation")
        nse <- NA
      }
      
      return(nse)
    }, error = function(e) {
      if (warnings) warning(paste("Error in NSE calculation:", e$message))
      return(NA)
    })
  }
  
  # Get simulated values
  Sim <- OutputsModel$Qsim
  
  # Get observations
  Obs <- InputsCrit$Obs
  
  # Apply transformation if needed
  if (!is.null(InputsCrit$transfo) && InputsCrit$transfo != "") {
    Sim <- do.call(what = paste0(".", toupper(InputsCrit$transfo)), 
                   args = list(Sim))
    Obs <- do.call(what = paste0(".", toupper(InputsCrit$transfo)), 
                   args = list(Obs))
  }
  
  # Calculate criterion
  CritValue <- .FUN(Obs = Obs, Sim = Sim)
  
  # airGR convention: negative NSE for minimization
  CritValue <- -CritValue
  
  # Handle NA or infinite values
  if (is.na(CritValue) || !is.finite(CritValue)) {
    CritValue <- 1e+08
  }
  
  # Create return structure
  OutputsCrit <- list(
    CritValue = CritValue,
    CritName = "NSE_Monthly", 
    CritBestValue = -1
  )
  
  # Set proper class (this is important!)
  class(OutputsCrit) <- c("OutputsCrit", "list")
  
  return(OutputsCrit)
}

# Apply proper class to the function (this is critical for airGR)
class(ErrorCrit_NSE_Monthly) <- c("FUN_CRIT", "function")
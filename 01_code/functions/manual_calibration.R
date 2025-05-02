# Function to manually calibrate
manual_calibration <- function(n_samples=50) {
  # Get monthly observations
  obs_monthly <- get_monthly_obs()
  cat("Number of monthly observations:", length(obs_monthly), "\n")
  
  # Define parameter ranges to explore
  if (model_2_use == "GR4J") {
    # Parameter ranges for GR4J
    param_ranges <- list(
      X1 = seq(100, 1000, length.out = 5),  # Production store capacity
      X2 = seq(-5, 5, length.out = 5),      # Water exchange coefficient
      X3 = seq(20, 300, length.out = 5),    # Routing store capacity
      X4 = seq(0.5, 4, length.out = 5),     # Unit hydrograph time base
      CemaNeigeParam1 = c(1),               # Snow module parameter
      CemaNeigeParam2 = c(1)                # Snow module parameter
    )
  } else if (model_2_use == "GR5J") {
    # Parameter ranges for GR5J
    param_ranges <- list(
      X1 = seq(100, 1000, length.out = 5),
      X2 = seq(-5, 5, length.out = 5),
      X3 = seq(20, 300, length.out = 5),
      X4 = seq(0.5, 4, length.out = 5),
      X5 = seq(0, 2, length.out = 5),
      CemaNeigeParam1 = c(1),
      CemaNeigeParam2 = c(1)
    )
  } else {
    # Parameter ranges for GR6J
    param_ranges <- list(
      X1 = seq(100, 1000, length.out = 5),
      X2 = seq(-5, 5, length.out = 5),
      X3 = seq(20, 300, length.out = 5),
      X4 = seq(0.5, 4, length.out = 5),
      X5 = seq(0, 2, length.out = 5),
      X6 = seq(0, 2, length.out = 5),
      CemaNeigeParam1 = c(1),
      CemaNeigeParam2 = c(1)
    )
  }
  
  # Get date range for simulation
  sim_dates <- basin_obs_ts$date[indRun_cal]
  
  # Set up for parameter sampling
  best_nse <- -Inf
  best_param <- NULL
  n_samples <- n_samples  # Number of parameter sets to try
  
  # Sample parameter sets
  for (i in 1:n_samples) {
    # Sample random parameters from ranges
    if (model_2_use == "GR4J") {
      param <- c(
        X1 = sample(param_ranges$X1, 1),
        X2 = sample(param_ranges$X2, 1),
        X3 = sample(param_ranges$X3, 1),
        X4 = sample(param_ranges$X4, 1),
        CemaNeigeParam1 = 1,
        CemaNeigeParam2 = 1
      )
    } else if (model_2_use == "GR5J") {
      param <- c(
        X1 = sample(param_ranges$X1, 1),
        X2 = sample(param_ranges$X2, 1),
        X3 = sample(param_ranges$X3, 1),
        X4 = sample(param_ranges$X4, 1),
        X5 = sample(param_ranges$X5, 1),
        CemaNeigeParam1 = 1,
        CemaNeigeParam2 = 1
      )
    } else {
      param <- c(
        X1 = sample(param_ranges$X1, 1),
        X2 = sample(param_ranges$X2, 1),
        X3 = sample(param_ranges$X3, 1),
        X4 = sample(param_ranges$X4, 1),
        X5 = sample(param_ranges$X5, 1),
        X6 = sample(param_ranges$X6, 1),
        CemaNeigeParam1 = 1,
        CemaNeigeParam2 = 1
      )
    }
    
    # Run model with parameters
    runResults <- run_model_with_params(param)
    
    # Calculate NSE with monthly data
    nse <- calculate_monthly_nse(obs_monthly, runResults$Qsim, sim_dates)
    
    cat("Sample", i, "NSE:", nse, "\n")
    
    # Update best if improved
    if (!is.na(nse) && nse > best_nse) {
      best_nse <- nse
      best_param <- param
      cat("New best NSE:", best_nse, "\n")
    }
  }
  
  # If no valid parameters found, use defaults
  if (is.null(best_param)) {
    cat("No valid parameters found. Using defaults.\n")
    if (model_2_use == "GR4J") {
      best_param <- c(X1 = 350, X2 = 0, X3 = 50, X4 = 0.5, 
                      CemaNeigeParam1 = 1, CemaNeigeParam2 = 1)
    } else if (model_2_use == "GR5J") {
      best_param <- c(X1 = 350, X2 = 0, X3 = 50, X4 = 0.5, X5 = 0,
                      CemaNeigeParam1 = 1, CemaNeigeParam2 = 1)
    } else {
      best_param <- c(X1 = 350, X2 = 0, X3 = 50, X4 = 0.5, X5 = 0, X6 = 0,
                      CemaNeigeParam1 = 1, CemaNeigeParam2 = 1)
    }
  }
  
  cat("Best parameters found with NSE =", best_nse, ":\n")
  print(best_param)
  
  return(best_param)
}
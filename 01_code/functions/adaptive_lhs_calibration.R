# Advanced Adaptive LHS Calibration Framework for Monthly Hydrological Modeling
# This approach bypasses airGR's calibration routines for more direct control

library(pacman)
p_load(here, tidyverse, lubridate, airGR, lhs, parallel, purrr, furrr)

#=============================================================================
# 1. ADAPTIVE LHS CALIBRATION FRAMEWORK
#=============================================================================

#' Advanced LHS-based parameter optimization with adaptive refinement
#' 
#' @param run_model Function that runs the model with given parameters
#' @param eval_function Function that evaluates model performance
#' @param param_ranges List with min/max for each parameter
#' @param n_initial_samples Number of initial LHS samples
#' @param n_iterations Number of refinement iterations
#' @param refinement_factor Proportion of best points to use for refinement
#' @param n_refined_samples Number of samples in each refinement iteration
#' @param parallel Whether to use parallel processing
#' @param n_cores Number of cores for parallel processing
#' 
#' @return List with best parameters and optimization history
adaptive_lhs_calibration <- function(run_model, eval_function, param_ranges, 
                                     n_initial_samples = 100, n_iterations = 5,
                                     refinement_factor = 0.2, n_refined_samples = 50,
                                     parallel = FALSE, n_cores = 4) {
  
  param_names <- names(param_ranges)
  n_params <- length(param_names)
  
  # Function to scale LHS points to parameter ranges
  scale_lhs <- function(lhs_points, ranges) {
    scaled_points <- lhs_points
    for (i in 1:ncol(lhs_points)) {
      param_name <- param_names[i]
      scaled_points[,i] <- ranges[[param_name]][1] + 
        lhs_points[,i] * (ranges[[param_name]][2] - ranges[[param_name]][1])
    }
    return(scaled_points)
  }
  
  # Function to convert scaled points to parameter lists
  points_to_params <- function(points) {
    params_list <- list()
    for (i in 1:nrow(points)) {
      params <- as.list(points[i,])
      names(params) <- param_names
      params_list[[i]] <- params
    }
    return(params_list)
  }
  
  # Function to evaluate a parameter set
  evaluate_param_set <- function(params) {
    # Run model with the parameter set
    model_results <- run_model(params)
    
    # Evaluate model performance
    performance <- eval_function(model_results)
    
    # Return results
    return(list(
      params = params,
      performance = performance,
      model_results = model_results
    ))
  }
  
  # Function to run evaluations (parallel or sequential)
  run_evaluations <- function(params_list) {
    if (parallel) {
      plan(multisession, workers = n_cores)
      results <- future_map(params_list, evaluate_param_set, .options = furrr_options(seed = TRUE))
      plan(sequential)
    } else {
      results <- map(params_list, evaluate_param_set)
    }
    return(results)
  }
  
  # Initialize history storage
  optimization_history <- list()
  
  # Initial LHS sampling
  cat("Generating initial Latin Hypercube Sample with", n_initial_samples, "points\n")
  initial_lhs <- randomLHS(n_initial_samples, n_params)
  initial_points <- scale_lhs(initial_lhs, param_ranges)
  initial_params_list <- points_to_params(initial_points)
  
  # Evaluate initial samples
  cat("Evaluating initial sample points...\n")
  initial_results <- run_evaluations(initial_params_list)
  optimization_history[[1]] <- initial_results
  
  # Find best parameters from initial sample
  performances <- sapply(initial_results, function(x) x$performance)
  best_idx <- which.max(performances)
  best_params <- initial_results[[best_idx]]$params
  best_performance <- performances[best_idx]
  
  cat("Initial best performance:", best_performance, "\n")
  
  # Adaptive refinement iterations
  current_param_ranges <- param_ranges
  
  for (iter in 1:n_iterations) {
    cat("\nRefinement iteration", iter, "of", n_iterations, "\n")
    
    # Select top performing parameter sets
    performances <- sapply(optimization_history[[iter]], function(x) x$performance)
    sorted_idx <- order(performances, decreasing = TRUE)
    n_top <- max(3, round(length(performances) * refinement_factor))
    top_idx <- sorted_idx[1:n_top]
    
    # Create refined parameter ranges based on top performers
    refined_ranges <- list()
    for (param in param_names) {
      top_values <- sapply(optimization_history[[iter]][top_idx], function(x) x$params[[param]])
      param_min <- max(current_param_ranges[[param]][1], min(top_values) * 0.8)
      param_max <- min(current_param_ranges[[param]][2], max(top_values) * 1.2)
      
      # Ensure range doesn't collapse
      if (param_max - param_min < (current_param_ranges[[param]][2] - current_param_ranges[[param]][1]) * 0.01) {
        center <- (param_max + param_min) / 2
        param_min <- center - (current_param_ranges[[param]][2] - current_param_ranges[[param]][1]) * 0.05
        param_max <- center + (current_param_ranges[[param]][2] - current_param_ranges[[param]][1]) * 0.05
      }
      
      refined_ranges[[param]] <- c(param_min, param_max)
    }
    
    # Generate refined LHS
    refined_lhs <- randomLHS(n_refined_samples, n_params)
    refined_points <- scale_lhs(refined_lhs, refined_ranges)
    refined_params_list <- points_to_params(refined_points)
    
    # Evaluate refined samples
    cat("Evaluating refined sample points...\n")
    refined_results <- run_evaluations(refined_params_list)
    optimization_history[[iter + 1]] <- refined_results
    
    # Update best parameters
    performances <- sapply(refined_results, function(x) x$performance)
    best_refined_idx <- which.max(performances)
    if (performances[best_refined_idx] > best_performance) {
      best_params <- refined_results[[best_refined_idx]]$params
      best_performance <- performances[best_refined_idx]
      cat("New best performance:", best_performance, "\n")
    } else {
      cat("No improvement in iteration", iter, ". Best remains:", best_performance, "\n")
    }
    
    # Update current parameter ranges
    current_param_ranges <- refined_ranges
  }
  
  # Flatten history for easier analysis
  all_results <- unlist(optimization_history, recursive = FALSE)
  
  # Extract all parameters and performances
  all_params <- lapply(all_results, function(x) x$params)
  all_performances <- sapply(all_results, function(x) x$performance)
  
  # Create performance data frame
  performance_df <- data.frame(
    iteration = rep(0:n_iterations, sapply(optimization_history, length)),
    performance = all_performances
  )
  
  # Parameter values data frame
  param_values <- matrix(unlist(all_params), ncol = length(param_names), byrow = TRUE)
  colnames(param_values) <- param_names
  param_df <- cbind(performance_df, as.data.frame(param_values))
  
  # Return complete results
  return(list(
    best_params = best_params,
    best_performance = best_performance,
    history = optimization_history,
    performance_df = performance_df,
    param_df = param_df
  ))
}

#=============================================================================
# 2. MONTHLY EVALUATION FRAMEWORK
#=============================================================================

#' Calculate Monthly NSE between observed and simulated values
#' 
#' @param sim_results Model simulation results
#' @param obs_monthly Monthly observed values
#' @param sim_dates Dates corresponding to simulation
#' @param metric Performance metric to use (NSE, KGE, etc.)
#' 
#' @return Numeric performance value (higher is better)
evaluate_monthly_performance <- function(sim_results, obs_monthly, sim_dates, 
                                         metric = "NSE") {
  # Extract simulated discharge
  Qsim <- sim_results$Qsim
  
  # Aggregate simulated values to monthly
  sim_df <- data.frame(
    date = sim_dates,
    Qsim = Qsim
  )
  
  # Calculate monthly means
  sim_df$month_key <- format(sim_df$date, "%Y-%m")
  sim_monthly <- aggregate(Qsim ~ month_key, data = sim_df, FUN = mean, na.rm = TRUE)
  
  # Create observed monthly dataframe
  obs_df <- data.frame(
    month_key = format(obs_monthly$date, "%Y-%m"),
    Qobs = obs_monthly$Q_mm_day
  )
  
  # Match simulated and observed months
  merged_data <- merge(sim_monthly, obs_df, by = "month_key")
  
  # Check if we have enough data
  if (nrow(merged_data) < 2) {
    return(-999) # Not enough data
  }
  
  # Calculate metric
  if (metric == "NSE") {
    # Nash-Sutcliffe Efficiency
    mean_obs <- mean(merged_data$Qobs, na.rm = TRUE)
    numerator <- sum((merged_data$Qsim - merged_data$Qobs)^2, na.rm = TRUE)
    denominator <- sum((merged_data$Qobs - mean_obs)^2, na.rm = TRUE)
    
    if (denominator > 0) {
      score <- 1 - numerator / denominator
    } else {
      score <- -999 # Avoid division by zero
    }
    
  } else if (metric == "KGE") {
    # Kling-Gupta Efficiency
    r <- cor(merged_data$Qsim, merged_data$Qobs)
    alpha <- sd(merged_data$Qsim) / sd(merged_data$Qobs)
    beta <- mean(merged_data$Qsim) / mean(merged_data$Qobs)
    score <- 1 - sqrt((r - 1)^2 + (alpha - 1)^2 + (beta - 1)^2)
    
  } else if (metric == "RMSE") {
    # Root Mean Square Error (negative for maximization)
    score <- -sqrt(mean((merged_data$Qsim - merged_data$Qobs)^2, na.rm = TRUE))
    
  } else {
    stop("Unsupported metric: ", metric)
  }
  
  return(score)
}

#=============================================================================
# 3. MODEL WRAPPER FUNCTIONS
#=============================================================================

#' Run GR model with parameters and return results
#' 
#' @param params List of model parameters
#' @param inputsModel InputsModel object for airGR
#' @param runOptions RunOptions object for airGR
#' @param model_type Type of model to run (GR4J, GR5J, GR6J)
#' 
#' @return Model run results
run_gr_model <- function(params, inputsModel, runOptions, model_type = "GR4J") {
  # Convert parameters list to vector with correct names
  param_vector <- unlist(params)
  
  # Run appropriate model
  if (model_type == "GR4J") {
    results <- airGR::RunModel_CemaNeigeGR4J(
      InputsModel = inputsModel,
      RunOptions = runOptions,
      Param = param_vector
    )
  } else if (model_type == "GR5J") {
    results <- airGR::RunModel_CemaNeigeGR5J(
      InputsModel = inputsModel,
      RunOptions = runOptions,
      Param = param_vector
    )
  } else if (model_type == "GR6J") {
    results <- airGR::RunModel_CemaNeigeGR6J(
      InputsModel = inputsModel,
      RunOptions = runOptions,
      Param = param_vector
    )
  } else {
    stop("Unsupported model type: ", model_type)
  }
  
  return(results)
}

#=============================================================================
# 4. VISUALIZATION FUNCTIONS
#=============================================================================

#' Plot calibration process and parameter convergence
#' 
#' @param calibration_results Results from adaptive_lhs_calibration
#' @param param_ranges Original parameter ranges
plot_calibration_diagnostics <- function(calibration_results, param_ranges) {
  # Performance over iterations
  p1 <- ggplot(calibration_results$performance_df, aes(x = iteration, y = performance)) +
    geom_violin(aes(group = iteration, fill = factor(iteration)), alpha = 0.7) +
    geom_jitter(width = 0.2, height = 0, alpha = 0.5) +
    scale_fill_viridis_d() +
    labs(
      title = "Performance Distribution by Iteration",
      x = "Iteration",
      y = "Performance (NSE)",
      fill = "Iteration"
    ) +
    theme_minimal()
  
  # Parameter convergence plots
  param_plots <- list()
  param_names <- names(param_ranges)
  
  for (param in param_names) {
    p <- ggplot(calibration_results$param_df, 
                aes(x = iteration, y = .data[[param]], color = performance)) +
      geom_point(alpha = 0.7) +
      geom_hline(yintercept = calibration_results$best_params[[param]], 
                 linetype = "dashed", color = "red") +
      scale_color_viridis_c() +
      labs(
        title = paste("Parameter:", param),
        x = "Iteration",
        y = "Value",
        color = "Performance"
      ) +
      theme_minimal()
    
    param_plots[[param]] <- p
  }
  
  # Display plots
  print(p1)
  for (p in param_plots) {
    print(p)
  }
}

#' Plot monthly comparison between observed and simulated discharge
#' 
#' @param sim_results Model simulation results
#' @param obs_monthly Monthly observed values
#' @param sim_dates Dates corresponding to simulation
#' @param title Plot title
plot_monthly_comparison <- function(sim_results, obs_monthly, sim_dates, title) {
  # Extract simulated discharge
  Qsim <- sim_results$Qsim
  
  # Aggregate simulated values to monthly
  sim_df <- data.frame(
    date = sim_dates,
    Qsim = Qsim
  )
  
  # Calculate monthly means
  sim_df$month_key <- format(sim_df$date, "%Y-%m")
  sim_monthly <- aggregate(Qsim ~ month_key, data = sim_df, FUN = mean, na.rm = TRUE)
  
  # Create observed monthly dataframe
  obs_df <- data.frame(
    month_key = format(obs_monthly$date, "%Y-%m"),
    Qobs = obs_monthly$Q_mm_day
  )
  
  # Match simulated and observed months
  merged_data <- merge(sim_monthly, obs_df, by = "month_key")
  
  # Add date column for plotting
  merged_data$date <- as.Date(paste0(merged_data$month_key, "-01"))
  merged_data <- merged_data[order(merged_data$date), ]
  
  # Calculate performance metrics
  nse <- evaluate_monthly_performance(
    sim_results, obs_monthly, sim_dates, metric = "NSE"
  )
  
  # RMSE
  rmse <- sqrt(mean((merged_data$Qsim - merged_data$Qobs)^2, na.rm = TRUE))
  
  # Percent bias
  pbias <- 100 * sum(merged_data$Qsim - merged_data$Qobs, na.rm = TRUE) / 
    sum(merged_data$Qobs, na.rm = TRUE)
  
  # Create plot
  plot(merged_data$date, merged_data$Qobs, 
       type = "o", col = "blue", pch = 16,
       xlab = "Date", ylab = "Discharge (mm/day)",
       main = paste0(title, " (NSE = ", round(nse, 3), 
                     ", RMSE = ", round(rmse, 3),
                     ", PBIAS = ", round(pbias, 1), "%)"))
  
  lines(merged_data$date, merged_data$Qsim, type = "o", col = "red", pch = 16)
  legend("topright", legend = c("Observed", "Simulated"), 
         col = c("blue", "red"), lty = 1, pch = 16)
  
  # Return comparison data
  return(invisible(merged_data))
}

#=============================================================================
# 5. IMPLEMENTATION EXAMPLE
#=============================================================================

#' Main workflow for advanced calibration
#' 
#' This example shows how to use the adaptive LHS framework
#' for hydrological model calibration with monthly observations.
atbashy_calibration_example <- function() {
  # Load required data
  Basin_Info <- readRDS(file.path(config$paths$data_path, "Basin_Info.rds"))
  cal_val_per <- readRDS(file.path(config$paths$data_path, "Calibration_Validation_Period.rds"))
  
  # Load discharge data
  q <- readRDS(file.path(paste0(config$paths$data_path,"/Discharge"), "q_cal_val.rds"))
  q <- q |> 
    mutate(date = ymd(date)) |> 
    mutate(date = floor_date(date, "month")) |>
    rename(Q_m3_sec = value) |>
    mutate(Q_mm_day = Q_m3_sec / as.numeric(Basin_Info$BasinArea_m2) * 10^3 * 3600 * 24) |> 
    dplyr::select(-data)
  
  # Load forcing data
  forcing_q_ts <- read_csv(file.path(config$paths$model_dir, "forcing_q_ts.csv"))
  basin_obs_ts <- forcing_q_ts |> 
    rename(Q_mm_day_no_glacier = Q_mm_day)
  
  # 1. Define time periods
  cal_val_per$calib_end <- ymd(cal_val_per$calib_end)
  indRun_cal <- which(basin_obs_ts$date >= cal_val_per$calib_start & 
                        basin_obs_ts$date <= cal_val_per$calib_end)
  indRun_cal_max <- max(indRun_cal)
  
  # Set warmup period
  n_warmup_years <- 3
  warmup_period_ind <- 1:(365*n_warmup_years + 1)
  # Adjust calibration period
  indRun_cal <- indRun_cal + length(warmup_period_ind)
  indRun_cal <- indRun_cal[indRun_cal <= indRun_cal_max]
  # Set validation period
  indRun_val <- which(basin_obs_ts$date >= cal_val_per$valid_start & 
                        basin_obs_ts$date <= cal_val_per$valid_end)
  
  # 2. Set model type
  model_type <- "GR4J"  # Can be "GR4J", "GR5J", or "GR6J"
  
  # 3. Load climate data
  hist_obs <- read_csv(file.path(config$paths$model_dir, "forcing/hist_obs_rsm.csv"), 
                       col_names = FALSE,
                       show_col_types = FALSE) 
  hist_obs <- hist_obs %>% dplyr::select(-ncol(hist_obs))
  
  # Process climate files
  source(here::here("01_code", "functions", "process_rsminerve_climate_files.R"))
  hist_obs_processed <- process_rsminerve_climate_files(hist_obs)
  
  # 4. Prepare model inputs
  if (model_type == "GR4J") {
    fun_model <- airGR::RunModel_CemaNeigeGR4J
  } else if (model_type == "GR5J") {
    fun_model <- airGR::RunModel_CemaNeigeGR5J
  } else if (model_type == "GR6J") {
    fun_model <- airGR::RunModel_CemaNeigeGR6J
  }
  
  inputsModel <- airGR::CreateInputsModel(
    FUN_MOD = fun_model, 
    DatesR = basin_obs_ts$date,
    Precip = basin_obs_ts$Ptot, 
    PotEvap = basin_obs_ts$PET,
    TempMean = basin_obs_ts$Temp, 
    HypsoData = Basin_Info$HypsoData,
    ZInputs = median(Basin_Info$HypsoData),
    NLayers = length(Basin_Info$ZLayers)
  )
  
  # Overwrite with our elevation band data
  inputsModel_ours <- inputsModel
  inputsModel_ours$LayerTempMean <- hist_obs_processed$T_bands |> as.list() 
  inputsModel_ours$LayerPrecip <- hist_obs_processed$P_bands |> as.list()
  
  # Compute solid precipitation fraction
  source(here::here("01_code", "functions", "solid_fraction_elevation_layer.R"))
  tas <- c(0.213, 0.489)  # Best parameters from experiments
  solid_frac_pr <- solid_fraction_elevation_layer(
    Basin_Info,
    inputsModel_ours$LayerTempMean, 
    tas_min = tas[1], 
    tas_max = tas[2]
  )
  
  inputsModel_ours$LayerFracSolidPrecip <- solid_frac_pr
  inputsModel <- inputsModel_ours
  
  # 5. Set up run options
  runOptions_cal <- airGR::CreateRunOptions(
    FUN_MOD = fun_model,
    InputsModel = inputsModel,
    IndPeriod_Run = indRun_cal,
    IniStates = NULL,
    IniResLevels = NULL,
    IndPeriod_WarmUp = warmup_period_ind,
    IsHyst = FALSE,
    warnings = TRUE
  )
  
  # 6. Prepare monthly observations for calibration
  obs_monthly_cal <- q %>%
    filter(date >= cal_val_per$calib_start & date <= cal_val_per$calib_end)
  
  # 7. Define parameter ranges
  # These ranges can be adjusted based on prior knowledge
  if (model_type == "GR4J") {
    param_ranges <- list(
      X1 = c(100, 1200),    # Production store capacity (mm)
      X2 = c(-5, 5),        # Groundwater exchange coefficient (mm/day)
      X3 = c(20, 300),      # Routing store capacity (mm)
      X4 = c(0.5, 5),       # Unit hydrograph time constant (days)
      CemaNeigeParam1 = c(0.5, 2), # Snow melt temperature factor
      CemaNeigeParam2 = c(0.5, 2)  # Snow cold content factor
    )
  } else if (model_type == "GR5J") {
    param_ranges <- list(
      X1 = c(100, 1200),
      X2 = c(-5, 5),
      X3 = c(20, 300),
      X4 = c(0.5, 5),
      X5 = c(0, 2),         # Inter-catchment exchange threshold (mm)
      CemaNeigeParam1 = c(0.5, 2),
      CemaNeigeParam2 = c(0.5, 2)
    )
  } else if (model_type == "GR6J") {
    param_ranges <- list(
      X1 = c(100, 1200),
      X2 = c(-5, 5),
      X3 = c(20, 300),
      X4 = c(0.5, 5),
      X5 = c(0, 2),
      X6 = c(0, 2),         # Exponential store depletion coefficient (-)
      CemaNeigeParam1 = c(0.5, 2),
      CemaNeigeParam2 = c(0.5, 2)
    )
  }
  
  # 8. Create model run and evaluation functions
  # Function to run the model
  run_model_wrapper <- function(params) {
    run_gr_model(params, inputsModel, runOptions_cal, model_type)
  }
  
  # Function to evaluate model performance
  eval_function_wrapper <- function(model_results) {
    evaluate_monthly_performance(
      model_results, 
      obs_monthly_cal, 
      basin_obs_ts$date[indRun_cal], 
      metric = "NSE"
    )
  }
  
  # 9. Run calibration
  cat("Starting adaptive LHS calibration...\n")
  calibration_results <- adaptive_lhs_calibration(
    run_model = run_model_wrapper,
    eval_function = eval_function_wrapper,
    param_ranges = param_ranges,
    n_initial_samples = 50,    # Number of initial LHS samples
    n_iterations = 3,          # Number of refinement iterations
    refinement_factor = 0.2,   # Top 20% used for refinement
    n_refined_samples = 30,    # Samples per refinement iteration
    parallel = FALSE           # Set to TRUE for parallel processing
  )
  
  # 10. Extract best parameters
  best_params <- calibration_results$best_params
  best_performance <- calibration_results$best_performance
  
  cat("\nBest parameters found:\n")
  print(best_params)
  cat("Best performance (NSE):", best_performance, "\n")
  
  # 11. Run model with best parameters
  param_vector <- unlist(best_params)
  if (model_type == "GR4J") {
    best_run_cal <- airGR::RunModel_CemaNeigeGR4J(
      InputsModel = inputsModel,
      RunOptions = runOptions_cal,
      Param = param_vector
    )
  } else if (model_type == "GR5J") {
    best_run_cal <- airGR::RunModel_CemaNeigeGR5J(
      InputsModel = inputsModel,
      RunOptions = runOptions_cal,
      Param = param_vector
    )
  } else if (model_type == "GR6J") {
    best_run_cal <- airGR::RunModel_CemaNeigeGR6J(
      InputsModel = inputsModel,
      RunOptions = runOptions_cal,
      Param = param_vector
    )
  }
  
  # 12. Plot calibration diagnostics
  plot_calibration_diagnostics(calibration_results, param_ranges)
  
  # 13. Plot calibration results
  cal_comparison <- plot_monthly_comparison(
    best_run_cal, 
    obs_monthly_cal, 
    basin_obs_ts$date[indRun_cal], 
    "Calibration Period"
  )
  
  # 14. Run validation
  runOptions_val <- airGR::CreateRunOptions(
    FUN_MOD = fun_model,
    InputsModel = inputsModel,
    IndPeriod_Run = indRun_val,
    IniStates = best_run_cal$StateEnd,
    IniResLevels = NULL
  )
  
  if (model_type == "GR4J") {
    best_run_val <- airGR::RunModel_CemaNeigeGR4J(
      InputsModel = inputsModel,
      RunOptions = runOptions_val,
      Param = param_vector
    )
  } else if (model_type == "GR5J") {
    best_run_val <- airGR::RunModel_CemaNeigeGR5J(
      InputsModel = inputsModel,
      RunOptions = runOptions_val,
      Param = param_vector
    )
  } else if (model_type == "GR6J") {
    best_run_val <- airGR::RunModel_CemaNeigeGR6J(
      InputsModel = inputsModel,
      RunOptions = runOptions_val,
      Param = param_vector
    )
  }
  
  # 15. Plot validation results
  obs_monthly_val <- q %>%
    filter(date >= cal_val_per$valid_start & date <= cal_val_per$valid_end)
  
  val_comparison <- plot_monthly_comparison(
    best_run_val, 
    obs_monthly_val, 
    basin_obs_ts$date[indRun_val], 
    "Validation Period"
  )
  
  # 16. Save results
  calibration_summary <- list(
    model_type = model_type,
    best_params = best_params,
    best_performance = best_performance,
    calibration_comparison = cal_comparison,
    validation_comparison = val_comparison,
    calibration_results = calibration_results
  )
  
  # Save calibrated parameters
  params_df <- data.frame(
    param_name = names(param_vector),
    param_value = as.numeric(param_vector),
    model = model_type
  )
  
  write.csv(params_df, file.path(config$paths$model_dir, "calibrated_parameters_lhs.csv"), 
            row.names = FALSE)
  
  # Save complete results object
  saveRDS(calibration_summary, 
          file.path(config$paths$model_dir, "calibration_summary_lhs.rds"))
  
  cat("\nCalibration process completed. Results saved.\n")
  
  return(calibration_summary)
}

# Run the example if this script is executed directly
if (!interactive()) {
  atbashy_calibration_example()
}

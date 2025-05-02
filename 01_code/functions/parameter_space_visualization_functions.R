# Advanced Parameter Space Visualization for Hydrological Modeling
# This script provides tools to visualize parameter interactions and sensitivity

library(pacman)
p_load(here, tidyverse, lubridate, ggplot2, plotly, viridis, GGally)

#=============================================================================
# 1. PARAMETER SPACE VISUALIZATION FUNCTIONS
#=============================================================================

#' Create interactive 3D parameter space visualization
#' 
#' @param param_df Dataframe with parameter values and performance
#' @param param_x First parameter to plot
#' @param param_y Second parameter to plot
#' @param param_z Third parameter to plot
#' @param metric Performance metric column name
#' 
#' @return A plotly 3D scatter plot object
visualize_3d_parameter_space <- function(param_df, param_x, param_y, param_z, 
                                         metric = "performance") {
  # Create 3D scatter plot
  plot_ly(param_df, 
          x = ~get(param_x), 
          y = ~get(param_y), 
          z = ~get(param_z),
          color = ~get(metric),
          colors = colorRamp(c("blue", "green", "red")),
          marker = list(size = 5, opacity = 0.7),
          type = "scatter3d", mode = "markers") %>%
    layout(
      title = "3D Parameter Space Visualization",
      scene = list(
        xaxis = list(title = param_x),
        yaxis = list(title = param_y),
        zaxis = list(title = param_z)
      )
    )
}

#' Create parameter sensitivity plots
#' 
#' @param param_df Dataframe with parameter values and performance
#' @param metric Performance metric column name
#' @param n_top Number of top performers to highlight
#' 
#' @return A grid of parameter sensitivity plots
plot_parameter_sensitivity <- function(param_df, metric = "performance", n_top = 10) {
  # Identify top performers
  param_df <- param_df %>%
    mutate(is_top = rank(-get(metric)) <= n_top)
  
  # Get parameter columns (excluding non-parameter columns)
  param_cols <- setdiff(colnames(param_df), c(metric, "iteration", "is_top"))
  
  # Create scatter plots for each parameter
  plots <- list()
  for (param in param_cols) {
    p <- ggplot(param_df, aes_string(x = param, y = metric)) +
      geom_point(aes(color = is_top, alpha = is_top), size = 3) +
      scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = "red")) +
      scale_alpha_manual(values = c("FALSE" = 0.4, "TRUE" = 0.9)) +
      geom_smooth(method = "loess", se = TRUE, color = "blue", alpha = 0.2) +
      labs(
        title = paste("Sensitivity to", param),
        x = param,
        y = metric
      ) +
      theme_minimal() +
      theme(legend.position = "none")
    
    plots[[param]] <- p
  }
  
  # Arrange in a grid
  gridExtra::grid.arrange(grobs = plots, ncol = 2)
  
  # Return invisibly
  invisible(plots)
}

#' Create parameter correlation matrix
#' 
#' @param param_df Dataframe with parameter values and performance
#' @param metric Performance metric column name
#' @param min_performance Minimum performance threshold to include
#' 
#' @return A correlation plot
plot_parameter_correlations <- function(param_df, metric = "performance", 
                                        min_performance = NULL) {
  # Filter by performance if threshold provided
  if (!is.null(min_performance)) {
    param_df <- param_df %>%
      filter(get(metric) >= min_performance)
  }
  
  # Get parameter columns
  param_cols <- setdiff(colnames(param_df), c(metric, "iteration", "is_top"))
  
  # Include performance in correlation analysis
  plot_cols <- c(param_cols, metric)
  
  # Create correlation plot
  GGally::ggcorr(
    param_df[, plot_cols], 
    method = c("complete.obs", "pearson"),
    label = TRUE,
    label_size = 3,
    hjust = 0.75,
    layout.exp = 2,
    color = "grey50"
  )
}

#' Create parameter pairs plot with performance coloring
#' 
#' @param param_df Dataframe with parameter values and performance
#' @param params Parameters to include (subset or all)
#' @param metric Performance metric column name
#' 
#' @return A GGally pairs plot
plot_parameter_pairs <- function(param_df, params = NULL, metric = "performance") {
  # Select parameters to include
  if (is.null(params)) {
    params <- setdiff(colnames(param_df), c(metric, "iteration", "is_top"))
  }
  
  # Include performance metric
  plot_cols <- c(params, metric)
  
  # Create custom plot function for diagonal panels
  my_diag <- function(data, mapping) {
    ggplot(data, mapping) +
      geom_density(fill = "skyblue", alpha = 0.5)
  }
  
  # Create custom plot function for upper panels
  my_upper <- function(data, mapping) {
    ggplot(data, mapping) +
      geom_point(size = 1, alpha = 0.5) +
      geom_smooth(method = "loess", se = FALSE, color = "red", size = 0.8)
  }
  
  # Create custom plot function for lower panels with performance coloring
  my_lower <- function(data, mapping) {
    ggplot(data, mapping) +
      geom_point(aes_string(color = metric), alpha = 0.7) +
      scale_color_viridis_c() +
      theme(legend.position = "none")
  }
  
  # Create pairs plot
  GGally::ggpairs(
    param_df[, plot_cols],
    columnLabels = plot_cols,
    upper = list(continuous = my_upper),
    diag = list(continuous = my_diag),
    lower = list(continuous = my_lower),
    progress = FALSE
  )
}

#' Create parameter evolution plots across iterations
#' 
#' @param param_df Dataframe with parameter values and performance
#' @param metric Performance metric column name
#' 
#' @return A faceted plot showing parameter convergence
plot_parameter_evolution <- function(param_df, metric = "performance") {
  # Get parameter columns
  param_cols <- setdiff(colnames(param_df), c(metric, "iteration", "is_top"))
  
  # Create long format dataframe for faceting
  param_long <- param_df %>%
    dplyr::select(all_of(c("iteration", param_cols, metric))) %>%
    pivot_longer(
      cols = all_of(param_cols),
      names_to = "parameter",
      values_to = "value"
    )
  
  # Create faceted plot
  ggplot(param_long, aes(x = iteration, y = value, color = get(metric))) +
    geom_point(alpha = 0.7) +
    geom_smooth(aes(group = 1), method = "loess", se = TRUE, color = "black", size = 1) +
    scale_color_viridis_c(name = metric) +
    facet_wrap(~ parameter, scales = "free_y", ncol = 2) +
    labs(
      title = "Parameter Evolution Across Iterations",
      x = "Iteration",
      y = "Parameter Value"
    ) +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill = "grey90"),
      strip.text = element_text(face = "bold")
    )
}

#' Create dynamic parameter trace plot 
#' 
#' @param param_df Dataframe with parameter values and performance
#' @param metric Performance metric column name
#' @param top_n Number of top performers to trace
#' 
#' @return A ggplot object showing parameter convergence paths
plot_parameter_traces <- function(param_df, param_x, param_y, 
                                  metric = "performance", top_n = 5) {
  # Add rank by performance
  param_df <- param_df %>%
    group_by(iteration) %>%
    mutate(rank = min_rank(-get(metric))) %>%
    ungroup()
  
  # Identify top performers in each iteration
  top_performers <- param_df %>%
    filter(rank <= top_n) %>%
    arrange(iteration, rank)
  
  # Create trace plot
  ggplot() +
    # Plot all points as background
    geom_point(data = param_df, 
               aes_string(x = param_x, y = param_y, color = metric),
               alpha = 0.3, size = 2) +
    # Highlight top performers
    geom_point(data = top_performers, 
               aes_string(x = param_x, y = param_y),
               color = "black", shape = 1, size = 3, stroke = 1.2) +
    # Add paths connecting top performers across iterations
    geom_path(data = top_performers, 
              aes_string(x = param_x, y = param_y, 
                         group = "iteration", color = metric),
              arrow = arrow(length = unit(0.2, "cm"), type = "closed"),
              size = 0.8) +
    # Add iteration labels
    geom_text(data = top_performers, 
              aes_string(x = param_x, y = param_y, 
                         label = "iteration"),
              hjust = -0.5, vjust = -0.5, size = 3) +
    # Use viridis color scale
    scale_color_viridis_c(name = metric) +
    # Add labels
    labs(
      title = paste("Parameter Convergence Traces:", param_x, "vs", param_y),
      subtitle = paste("Top", top_n, "performers by", metric),
      x = param_x,
      y = param_y
    ) +
    theme_minimal()
}

#' Plot parameter uncertainty bands
#' 
#' @param calibration_results Calibration results list
#' @param n_best Number of best parameter sets to use
#' @param inputsModel InputsModel object for airGR
#' @param runOptions RunOptions object for airGR
#' @param obs_monthly Monthly observed values
#' @param model_type Model type (GR4J, GR5J, GR6J)
#' 
#' @return A ggplot with uncertainty bands
plot_parameter_uncertainty <- function(calibration_results, n_best = 10,
                                       inputsModel, runOptions, obs_monthly,
                                       model_type = "GR4J") {
  # Extract parameter dataframe
  param_df <- calibration_results$param_df
  
  # Get top parameter sets
  top_params <- param_df %>%
    arrange(desc(performance)) %>%
    head(n_best)
  
  # Run simulations for each parameter set
  sim_results <- list()
  for (i in 1:nrow(top_params)) {
    # Create parameter vector
    param_names <- setdiff(colnames(top_params), c("iteration", "performance"))
    param_vector <- as.numeric(top_params[i, param_names])
    names(param_vector) <- param_names
    
    # Run model
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
    }
    
    sim_results[[i]] <- results
  }
  
  # Extract simulation dates
  sim_dates <- inputsModel$DatesR[runOptions$IndPeriod_Run]
  
  # Process all simulations to monthly
  monthly_sims <- list()
  for (i in 1:length(sim_results)) {
    # Extract simulated discharge
    Qsim <- sim_results[[i]]$Qsim
    
    # Aggregate to monthly
    sim_df <- data.frame(
      date = sim_dates,
      Qsim = Qsim
    )
    sim_df$month_key <- format(sim_df$date, "%Y-%m")
    monthly_sim <- aggregate(Qsim ~ month_key, data = sim_df, FUN = mean, na.rm = TRUE)
    
    # Store with parameter set ID
    monthly_sim$param_set <- i
    monthly_sims[[i]] <- monthly_sim
  }
  
  # Combine all simulations
  all_sims <- do.call(rbind, monthly_sims)
  
  # Create observed monthly dataframe
  obs_df <- data.frame(
    month_key = format(obs_monthly$date, "%Y-%m"),
    Qobs = obs_monthly$Q_mm_day
  )
  
  # Calculate statistics for each month
  sim_stats <- all_sims %>%
    group_by(month_key) %>%
    summarize(
      mean = mean(Qsim, na.rm = TRUE),
      median = median(Qsim, na.rm = TRUE),
      q05 = quantile(Qsim, 0.05, na.rm = TRUE),
      q25 = quantile(Qsim, 0.25, na.rm = TRUE),
      q75 = quantile(Qsim, 0.75, na.rm = TRUE),
      q95 = quantile(Qsim, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Merge with observations
  plot_data <- left_join(sim_stats, obs_df, by = "month_key")
  
  # Add date column for plotting
  plot_data$date <- as.Date(paste0(plot_data$month_key, "-01"))
  plot_data <- plot_data[order(plot_data$date), ]
  
  # Create plot
  ggplot(plot_data, aes(x = date)) +
    # Add uncertainty bands
    geom_ribbon(aes(ymin = q05, ymax = q95), fill = "skyblue", alpha = 0.3) +
    geom_ribbon(aes(ymin = q25, ymax = q75), fill = "skyblue", alpha = 0.5) +
    # Add median line
    geom_line(aes(y = median), color = "blue", size = 1) +
    # Add observed points
    geom_point(aes(y = Qobs), color = "red", size = 3, alpha = 0.7) +
    geom_line(aes(y = Qobs), color = "red", size = 0.8, alpha = 0.7) +
    # Add labels
    labs(
      title = "Parameter Uncertainty Analysis",
      subtitle = paste("Based on top", n_best, "parameter sets"),
      x = "Date",
      y = "Discharge (mm/day)"
    ) +
    # Customize theme
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold"),
      legend.position = "bottom"
    )
}

#=============================================================================
# 2. INTERACTIVE DASHBOARD FUNCTION  
#=============================================================================

#' Create an interactive dashboard for parameter analysis
#' (Note: This would require additional libraries like shiny in a real app)
#' 
#' @param calibration_results Results from adaptive LHS calibration
#' @param param_ranges Original parameter ranges
parameter_analysis_dashboard <- function(calibration_results, param_ranges) {
  # This is a placeholder for a full shiny dashboard
  # In practice, you would create a shiny app with these components
  
  cat("Creating parameter analysis dashboard...\n")
  cat("This function would create a Shiny dashboard with:\n")
  cat(" - Parameter sensitivity plots\n")
  cat(" - 3D parameter space visualization\n")
  cat(" - Parameter correlation matrix\n")
  cat(" - Parameter evolution plots\n")
  cat(" - Uncertainty analysis\n")
  cat(" - Interactive parameter selection\n")
  
  # Basic visualizations as demonstration
  param_df <- calibration_results$param_df
  
  # 1. Parameter sensitivity plots
  plot_parameter_sensitivity(param_df)
  
  # 2. Parameter correlation matrix
  plot_parameter_correlations(param_df)
  
  # 3. Parameter pairs plot (first 4 parameters only for clarity)
  param_cols <- setdiff(colnames(param_df), c("performance", "iteration", "is_top"))
  if (length(param_cols) > 4) {
    param_subset <- param_cols[1:4]
  } else {
    param_subset <- param_cols
  }
  plot_parameter_pairs(param_df, param_subset)
  
  # 4. Parameter evolution plot
  plot_parameter_evolution(param_df)
  
  # Return parameter dataframe for further analysis
  return(invisible(param_df))
}

#=============================================================================
# 3. EXAMPLE USAGE
#=============================================================================

# Example usage (assuming calibration_results is available):
#
# # Load calibration results
# calibration_results <- readRDS(file.path(config$paths$model_dir, "calibration_summary_lhs.rds"))
# 
# # Original parameter ranges used in calibration
# param_ranges <- list(
#   X1 = c(100, 1200),
#   X2 = c(-5, 5),
#   X3 = c(20, 300),
#   X4 = c(0.5, 5),
#   CemaNeigeParam1 = c(0.5, 2),
#   CemaNeigeParam2 = c(0.5, 2)
# )
# 
# # Create visualizations
# plot_parameter_sensitivity(calibration_results$param_df)
# 
# plot_parameter_pairs(calibration_results$param_df)
# 
# # Create 3D visualization of key parameters
# visualize_3d_parameter_space(
#   calibration_results$param_df,
#   param_x = "X1",
#   param_y = "X3", 
#   param_z = "X4"
# )
# 
# # Create uncertainty plot
# plot_parameter_uncertainty(
#   calibration_results,
#   n_best = 10,
#   inputsModel = inputsModel,
#   runOptions = runOptions_cal,
#   obs_monthly = obs_monthly_cal,
#   model_type = "GR4J"
# )
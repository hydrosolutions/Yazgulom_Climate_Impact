#' Create Multi-Panel Hydrograph Plots for Climate Scenarios
#'
#' @description
#' Creates a multi-panel plot showing mean hydrographs for different climate scenarios
#' and time periods. Each panel represents one climate scenario (SSP) compared against
#' observational data, with different line types for different time periods.
#'
#' @param data A data frame containing columns:
#'   \itemize{
#'     \item date: Date of observation/projection
#'     \item Q_mm_day: Discharge in mm/day
#'     \item scenario: Climate scenario (ssp126, ssp245, ssp370, ssp585, observation)
#'     \item period: Time period (p0, p1, p2, p3)
#'   }
#' @param aggregation Character string specifying temporal aggregation ("monthly" or "weekly")
#'
#' @return A ggplot2 object containing the multi-panel hydrograph plot
#'
#' @examples
#' \dontrun{
#' plot_flexible_hydrograph(hydro_data, aggregation = "monthly")
#' plot_flexible_hydrograph(hydro_data, aggregation = "weekly")
#' }
#'
plot_flexible_hydrograph <- function(data, aggregation = "monthly") {
  # Input validation for aggregation parameter
  if (!aggregation %in% c("monthly", "weekly")) {
    stop("aggregation must be either 'monthly' or 'weekly'")
  }
  
  # Calculate temporal means based on chosen aggregation (monthly/weekly)
  temporal_means <- data %>%
    mutate(
      time_unit = if(aggregation == "monthly") {
        month(date)
      } else {
        week(date)
      }
    ) %>%
    group_by(time_unit, scenario, period) %>%
    summarize(mean_Q = mean(Q_mm_day), .groups = "drop")
  
  # Define visual parameters for plotting
  # Line types for different time periods
  period_linetypes <- c(
    "p0" = "solid",     # 2000-2020
    "p1" = "dashed",    # 2021-2040
    "p2" = "dotdash",   # 2041-2070
    "p3" = "dotted"     # 2071-2100
  )
  
  # Color scheme for different scenarios
  scenario_colors_with_obs <- c(
    "ssp126" = "#2196F3",  # Light Blue
    "ssp245" = "#4CAF50",  # Light Green
    "ssp370" = "#FFA000",  # Light Orange
    "ssp585" = "#F44336",  # Light Red
    "observation" = "black" # Black
  )
  
  # Get unique scenarios excluding observations for panel creation
  scenarios <- unique(temporal_means$scenario[temporal_means$scenario != "observation"])
  
  # Calculate optimal layout for subplots
  n_scenarios <- length(scenarios)
  n_rows <- ceiling(sqrt(n_scenarios))
  n_cols <- ceiling(n_scenarios/n_rows)
  
  # Set x-axis formatting based on temporal aggregation
  if(aggregation == "monthly") {
    x_breaks <- 1:12
    x_labels <- month.abb
    x_limits <- c(1, 12)
  } else {
    x_breaks <- c(1, 13, 26, 39, 52)
    x_labels <- c("Jan", "Apr", "Jul", "Oct", "Dec")
    x_limits <- c(1, 52)
  }
  
  # Create individual plots for each scenario
  scenario_plots <- list()
  for(scen in scenarios) {
    # Filter data for current scenario and observations
    plot_data <- temporal_means %>%
      filter(scenario %in% c(scen, "observation"))
    
    # Generate individual scenario plot with consistent styling
    p <- ggplot(plot_data, aes(x = time_unit, y = mean_Q, 
                               color = scenario,
                               linetype = period)) +
      geom_line(linewidth = 0.8) +
      scale_color_manual(
        values = c("observation" = "black", 
                   setNames(scenario_colors_with_obs[scen], scen)),
        guide = "none"
      ) +
      scale_linetype_manual(
        values = period_linetypes,
        name = "Period",
        labels = c("2000-2020", "2021-2040", "2041-2070", "2071-2100"),
        guide = guide_legend(
          override.aes = list(
            color = c("black", rep(scenario_colors_with_obs[scen], 3))
          )
        )
      ) +
      scale_x_continuous(
        breaks = x_breaks,
        labels = x_labels,
        limits = x_limits
      ) +
      labs(
        title = paste("Scenario:", toupper(scen)),
        x = "Month",
        y = "Mean Discharge [mm/day]"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 12, face = "bold"),
        axis.title = element_text(size = 10),
        axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 8, face = "bold"),
        legend.text = element_text(size = 7),
        legend.position = c(0.02, 0.98),
        legend.justification = c(0, 1),
        legend.margin = margin(0, 0, 0, 0),
        legend.spacing = unit(0, "pt"),
        legend.background = element_rect(fill = "white", color = NA),
        legend.key.size = unit(0.8, "lines"),
        panel.grid.minor = element_blank(),
        panel.border = element_rect(fill = NA, color = "gray80")
      )
    
    scenario_plots[[scen]] <- p
  }
  
  # Create dynamic title based on aggregation type
  plot_title <- paste("Per Period Mean", if(aggregation == "monthly") "Monthly" else "Weekly",
                      "Hydrographs by Scenario")
  
  # Combine all scenario plots into final multi-panel plot
  combined_plot <- wrap_plots(scenario_plots, ncol = n_cols) +
    plot_annotation(
      title = plot_title,
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    )
  
  return(combined_plot)
}

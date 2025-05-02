#' Create Climate Forcing Time Series Plot
#' 
#' @description
#' Creates a professional time series plot comparing observed and projected climate data
#' across different climate change scenarios. The function can visualize either temperature
#' or precipitation data for specific elevation bands.
#' 
#' @param all_data A data frame containing climate forcing data with columns:
#'   - date: Date of measurement
#'   - station: Station identifier (format: "{river_name}_{elevation_band}")
#'   - sensor: Type of measurement ('T' for temperature, 'P' for precipitation)
#'   - value: Measured value
#'   - model: Climate model identifier or 'obs' for observations
#'   - scenario: Climate scenario identifier (ssp126, ssp245, ssp370, ssp585)
#' @param model_sel String specifying the climate model to plot (e.g., "GFDL", "IPSL")
#' @param el_band String or numeric specifying the elevation band to plot
#' @param sensor_type Character ('T' or 'P') specifying the type of measurement to plot
#' @param y_max_value Numeric specifying the maximum value for the y-axis
#' 
#' @return A ggplot object showing the time series of climate forcing data
plot_forcing_data <- function(all_data, model_sel, el_band, sensor_type, y_max_value) {
  # Define color scheme for different SSP scenarios
  # Uses ColorBrewer palette for better visual distinction and accessibility
  scenario_colors <- c(
    "ssp126" = "#4575B4",  # Blue for low emission scenario
    "ssp245" = "#74ADD1",  # Light Blue for moderate emission
    "ssp370" = "#F46D43",  # Orange for high emission
    "ssp585" = "#D73027",  # Red for very high emission
    "obs" = "#000000"      # Black for historical observations
  )
  
  # Create descriptive labels for the scenarios in the legend
  scenario_labels <- c(
    "ssp126" = "SSP1-2.6",
    "ssp245" = "SSP2-4.5",
    "ssp370" = "SSP3-7.0",
    "ssp585" = "SSP5-8.5",
    "obs" = "Observations"
  )
  
  # Set appropriate y-axis label based on the sensor type
  # Includes units for clarity
  y_label <- if(sensor_type == "T") {
    "Temperature (°C)"
  } else {
    "Annual Precipitation (mm)"
  }
  
  # Main plot creation
  all_data |> 
    # Filter the dataset for relevant observations
    filter(
      station == paste0(river_name, "_", el_band),    # Select specific elevation band
      sensor == sensor_type,                          # Filter for T or P
      model %in% c("obs", model_sel)                  # Include observations and selected model
    ) |>
    # Calculate annual statistics
    group_by(scenario) |>
    timetk::summarize_by_time(
      .date_var = date,
      .by = "year",
      # Sum for precipitation, mean for temperature
      value = ifelse(sensor_type == "P", 
                     sum(value),    # Annual precipitation sum
                     mean(value))   # Annual temperature mean
    ) |> 
    # Create the base plot
    ggplot(aes(x = date, y = value, color = scenario)) +
    # Add lines with different styling for observations vs projections
    geom_line(
      aes(linetype = scenario == "obs"),
      linewidth = 0.8
    ) +
    # Add points only for observations for better visibility
    geom_point(
      data = . %>% filter(scenario == "obs"),
      size = 1
    ) +
    # Configure line types (solid for all, but handled separately for clarity)
    scale_linetype_manual(
      values = c("TRUE" = "solid", "FALSE" = "solid"),
      guide = "none"
    ) +
    # Add informative labels
    labs(
      title = paste("Climate Projections for", river_name),
      subtitle = sprintf("%s Model, Elevation Band %s", model_sel, el_band),
      x = "Year",
      y = y_label,
      color = "Scenario"
    ) +
    # Apply and customize theme
    theme_minimal() +
    theme(
      # Text formatting
      text = element_text(family = "Arial"),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 10, face = "bold"),
      axis.text = element_text(size = 9),
      # Legend formatting
      legend.title = element_text(face = "bold"),
      legend.text = element_text(size = 9),
      legend.position = "bottom",
      legend.box = "horizontal",
      # Grid formatting
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey90")
    ) +
    # Set plot boundaries and scale
    coord_cartesian(
      ylim = c(0, y_max_value)
    ) +
    # Configure x-axis date breaks
    scale_x_date(
      date_breaks = "10 years",   # Show tick every 10 years
      date_labels = "%Y"          # Show years as YYYY
    ) +
    # Apply custom colors and labels for scenarios
    scale_color_manual(
      values = scenario_colors,
      labels = scenario_labels,
      name = "Scenario"
    )
}
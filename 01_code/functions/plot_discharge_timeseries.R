#' Plot Discharge Time Series with Customized Styling
#'
#' @description
#' Creates a professional-looking time series plot for river discharge data with
#' customized styling, appropriate axis breaks, and formatting. The function is
#' designed to visualize gap-filled monthly mean river discharge data.
#'
#' @param data A tibble or data frame containing discharge time series data.
#' @param date_col Character string specifying the column name containing dates. Default: "date"
#' @param value_col Character string specifying the column name containing discharge values. Default: "value"
#' @param gauge_name Optional character string with the gauge station name for the plot title.
#' @param gauge_code Optional character string with the gauge station code for the plot title.
#' @param y_label Character string for the y-axis label. Default: "Discharge [m³/s]"
#' @param theme_font Character string specifying the font family for the plot. Default: "Arial"
#'
#' @return A ggplot2 object representing the discharge time series plot.
#'
#' @examples
#' # Basic usage with default parameters
#' # plot_discharge_timeseries(discharge_data)
#' 
#' # With custom gauge information
#' # plot_discharge_timeseries(discharge_data, 
#' #                          gauge_name = "Darband", 
#' #                          gauge_code = "17084")
#'
#' @import ggplot2
#' @import dplyr
#' @import lubridate
#' @import scales
#'
plot_discharge_timeseries <- function(data, 
                                    date_col = "date",
                                    value_col = "value",
                                    gauge_name = NULL,
                                    gauge_code = NULL,
                                    y_label = "Discharge [m³/s]",
                                    theme_font = "Arial") {
  
  # Create title based on gauge info
  plot_title <- if (!is.null(gauge_name) && !is.null(gauge_code)) {
    paste0("Gap-Filled Monthly Mean River Discharge\n", gauge_name, " (", gauge_code, ")")
  } else {
    "Gap-Filled Monthly Mean River Discharge"
  }
  
  # Calculate date limits and breaks every 5 years
  date_range <- range(data[[date_col]], na.rm = TRUE)
  start_year <- floor_date(date_range[1], "5 years")
  end_year <- ceiling_date(date_range[2], "5 years")
  
  # Create the plot
  p <- ggplot(data, aes(x = .data[[date_col]], y = .data[[value_col]])) +
    # Add line and points
    geom_line(color = "#2b8cbe", linewidth = 0.6, alpha = 0.8) +  # Blue line for time series
    geom_point(color = "#2b8cbe", size = .5, alpha = 0.6) +       # Points for individual observations
    
    # Customize theme
    theme_minimal(base_family = theme_font) +
    theme(
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),  # Centered bold title
      plot.subtitle = element_text(size = 12, hjust = 0.5),              # Centered subtitle
      axis.title = element_text(size = 11),                              # Axis titles
      axis.text = element_text(size = 10),                               # Axis text
      panel.grid.minor = element_line(color = "gray90"),                 # Subtle minor gridlines
      panel.grid.major = element_line(color = "gray85"),                 # Subtle major gridlines
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20)              # Plot margins for spacing
    ) +
    
    # Customize axes
    scale_x_date(
      breaks = seq(start_year, end_year, by = "10 years"),  # Date breaks every 10 years
      date_labels = "%Y",                                   # Show only years on x-axis
      expand = expansion(mult = c(0.02, 0.02))              # Small expansion at edges
    ) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 1),          # Clean number formatting
      expand = expansion(mult = c(0.02, 0.05))              # Small expansion, more at top
    ) +
    
    # Add labels
    labs(
      title = plot_title,                                   # Dynamic title based on inputs
      x = "Date",                                           # X-axis label
      y = y_label                                           # Customizable Y-axis label
    )
  
  return(p)
}
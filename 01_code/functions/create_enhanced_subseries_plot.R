#' Create Enhanced Subseries Plot for Hydrological Time Series
#'
#' @description
#' Creates a multi-panel subseries plot showing monthly discharge patterns with enhanced
#' visual elements including trend lines, confidence intervals, and customized styling.
#' Each panel shows data for a different month across all years in the dataset.
#'
#' @param data A tibble or data frame containing date and discharge value columns.
#'             Must have columns named 'date' and 'value'.
#' @param station_name Character string with the station name for plot title. Default: "Darband (17048)"
#' @param start_year Numeric value for the first year in the data series. Default: 1931
#' @param end_year Numeric value for the last year in the data series. Default: 2023
#'
#' @return A ggplot2 object displaying the enhanced subseries plot.
#'
#' @examples
#' # Assuming q_dly is a data frame with date and discharge values
#' # plot <- create_enhanced_subseries_plot(q_dly, "Darband (17084)", 1940, 2023)
#'
#' @import ggplot2
#' @import timetk
#' @import forecast
#' @import dplyr
#'
create_enhanced_subseries_plot <- function(data, station_name = "Atschikomandi (16076)", 
                                           start_year = 1931, end_year = 2023) {
  # Create the base subseries plot with enhancements
  subseries_plot <- data |> 
    # Aggregate data to monthly averages
    timetk::summarize_by_time(.date_var = date, .by = "month", value = mean(value)) |>
    # Convert to time series object with monthly frequency
    timetk::tk_ts(frequency = 12) |> 
    # Create base subseries plot without year labels
    forecast::ggsubseriesplot(year.labels = FALSE) +
    
    # Add mean points for each month to highlight average values
    stat_summary(fun = mean, 
                 geom = "point", 
                 color = "darkblue", 
                 size = .2,
                 alpha = 0.7) +
    
    # Add trend lines with confidence intervals to show temporal changes
    geom_smooth(method = "lm", 
                color = "red", 
                size = 0.8,
                alpha = 0.2) +
    
    # Customize labels and title for better readability and context
    labs(x = 'Month',
         y = expression(paste("Discharge [m"^3, "/s]")),
         title = paste(start_year, "-", end_year, "Subseries Plot with Trends,", station_name),
         subtitle = "Monthly averages (blue points) with linear trends (red lines) and confidence intervals") +
    
    # Set minimal theme as base for clean visualization
    theme_minimal() +
    theme(
      # Title and subtitle formatting for emphasis and readability
      plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 10, hjust = 0.5, color = "grey40"),
      
      # Grid lines for better visual guidance while maintaining subtlety
      panel.grid.major = element_line(color = "grey90", size = 0.3),
      panel.grid.minor = element_line(color = "grey95", size = 0.2),
      
      # Axis formatting for clarity
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9),
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      
      # Panel spacing for visual separation between months
      panel.spacing = unit(1, "lines"),
      
      # Plot margins for overall balance
      plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
    ) +
    
    # Scale modifications for better labeling and spacing
    scale_x_continuous(breaks = 1:12,
                       labels = month.abb,  # Use abbreviated month names
                       expand = expansion(mult = 0.05)) +
    # Add space above and below the data for visual comfort
    scale_y_continuous(expand = expansion(mult = c(0.05, 0.1)))
  
  # Return the completed plot object
  return(subseries_plot)
}
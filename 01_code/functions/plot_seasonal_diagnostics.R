#' Create Seasonal Diagnostics Plots for Hydrological Time Series
#'
#' @description
#' Creates a multi-panel visualization of river discharge data that includes a time series plot
#' with trend line and monthly distribution diagnostics. The function helps analyze seasonal
#' patterns, trends, and variability in discharge data.
#'
#' @param df A tibble or data frame containing at least two columns:
#'           - 'date': dates in Date or character format
#'           - 'value': numeric discharge values in m³/s
#'
#' @return A patchwork object combining multiple ggplot2 plots showing:
#'         1. Time series with smoothed trend line
#'         2. Monthly boxplots showing distribution of discharge values
#'
#' @examples
#' # Assuming q_dly is a data frame with date and discharge values
#' # seasonal_plot <- plot_seasonal_diagnostics(q_dly)
#'
#' @import dplyr
#' @import lubridate
#' @import ggplot2
#' @import patchwork
#'
plot_seasonal_diagnostics <- function(df) {
  # Ensure date column is in Date format and extract date components
  df <- df %>%
    mutate(
      date = as.Date(date),                                    # Convert to Date type if not already
      year = year(date),                                       # Extract year for potential grouping
      month = month(date),                                     # Extract numeric month value
      month_name = factor(month.abb[month], levels = month.abb) # Convert to ordered factor with month abbreviations
    )
  
  # Calculate summary statistics for each month
  monthly_stats <- df %>%
    group_by(month, month_name) %>%
    summarize(
      mean_discharge = mean(value, na.rm = TRUE),      # Central tendency
      median_discharge = median(value, na.rm = TRUE),  # Alternative central tendency (robust to outliers)
      q25 = quantile(value, 0.25, na.rm = TRUE),       # Lower quartile for IQR
      q75 = quantile(value, 0.75, na.rm = TRUE),       # Upper quartile for IQR
      min_discharge = min(value, na.rm = TRUE),        # Extreme minimum
      max_discharge = max(value, na.rm = TRUE),        # Extreme maximum
      .groups = "drop"                                 # Avoid grouped tibble output
    )
  
  # Plot 1: Full time series with trend analysis
  p1 <- ggplot(df, aes(x = date, y = value)) +
    # Raw data as time series line
    geom_line(color = "steelblue", alpha = 0.7) +
    # Add smoothed trend line with confidence interval
    geom_smooth(method = "loess", color = "red", fill = "pink", alpha = 0.3) +
    # Labels and title
    labs(
      x = "Year",
      y = expression("Discharge (m"^3*"/s)"),
      title = "Atbashy River Discharge Time Series"
    ) +
    # Clean, minimal theme
    theme_minimal() +
    # Custom theme elements
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold title
      axis.text = element_text(size = 9),                     # Smaller axis text for readability
      axis.title = element_text(size = 10)                    # Adjusted axis title size
    )
  
  # Plot 2: Monthly distribution as boxplots
  p2 <- ggplot(df, aes(x = month_name, y = value)) +
    # Boxplots showing distribution by month
    geom_boxplot(fill = "lightblue", alpha = 0.7) +
    # Overlay mean values as a line for comparison
    geom_line(data = monthly_stats, 
              aes(x = month_name, y = mean_discharge, group = 1),
              color = "red", linetype = "dashed") +
    labs(
      x = "Month",
      y = expression("Discharge (m"^3*"/s)"),
      title = "Monthly Distribution of Discharge"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Create monthly means with confidence intervals
  p3 <- ggplot(monthly_stats, aes(x = month_name)) +
    geom_ribbon(aes(ymin = q25, ymax = q75), 
                fill = "lightblue", alpha = 0.3) +
    geom_line(aes(y = mean_discharge, group = 1), 
              color = "steelblue", size = 1) +
    geom_point(aes(y = mean_discharge), 
               color = "steelblue", size = 3) +
    labs(
      x = "Month",
      y = expression("Discharge (m"^3*"/s)"),
      title = "Monthly Mean Discharge with Quartile Range"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.text = element_text(size = 9),
      axis.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Combine plots using patchwork
  library(patchwork)
  combined_plot <- (p1 / p2) +
    plot_layout(heights = c(1, 1)) +
    plot_annotation(
      title = "Atbashy River Discharge Seasonal Diagnostics",
      theme = theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
      )
    )
  
  return(combined_plot)
}
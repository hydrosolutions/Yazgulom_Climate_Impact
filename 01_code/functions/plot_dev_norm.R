#' Create Discharge Deviation Plot
#' 
#' This function creates a bar plot showing deviations from a long-term norm for 
#' discharge measurements. Positive deviations are shown in blue, negative in red.
#' 
#' @param data A data frame containing at least the following columns:
#'   - mean: Numeric column containing discharge measurements in m3/s
#'   - grouping: Factor column indicating the time period category
#' @param long_term_norm Numeric value representing the long-term norm for comparison
#' @param timeperiod String or factor indicating which grouping to plot (e.g., "Cold Season", "Warm Season")
#' @param title String containing the plot title
#' 
#' @return A ggplot object showing the discharge deviations from the norm over time
#'
#' @details The function calculates deviations from the provided long-term norm and
#'          creates a bar plot where positive deviations are shown in blue and
#'          negative deviations in red. The x-axis spans from 1931 to 2023.
#'          The y-axis shows the deviation in m3/s.
plot_dev_norm <- function(data, long_term_norm, timeperiod, title) {
  # Calculate deviations from norm
  data_norm_dev <- data |> 
    mutate(
      norm_dev = mean - long_term_norm
    ) |> 
    na.omit()
  
  # Create deviation plot
  plot_norm_dev <- data_norm_dev |> 
    # Filter for specified time period
    filter(grouping == timeperiod) |> 
    # Create sequence of years for x-axis
    mutate(year = seq(1931, 2023, 1)) |>
    # Initialize plot
    ggplot(aes(x = year, y = norm_dev)) +
    # Create bars colored by deviation direction
    ggplot2::geom_bar(
      ggplot2::aes(fill = norm_dev < 0),
      stat = "identity"
    ) +
    # Set color scheme (red for negative, blue for positive)
    ggplot2::scale_fill_manual(
      guide = "none",
      breaks = c(TRUE, FALSE),
      values = c("red", "blue")
    ) +
    # Add labels and title
    xlab("Year") +
    ylab("Q [m3/s]") +
    ggtitle(title) +
    theme_minimal()
  
  return(plot_norm_dev)
}
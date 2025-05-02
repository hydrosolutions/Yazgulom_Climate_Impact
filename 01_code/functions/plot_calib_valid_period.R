#' Plot Time Series of Specific Discharge with Model Periods
#' 
#' Creates a ggplot visualization of specific discharge data, distinguishing between
#' warmup, calibration, and validation periods using different colors and markers.
#'
#' @param q_dly A data frame containing:
#'   \item{date}{Date column for the time series}
#'   \item{Qmm}{Numeric column with specific discharge values in mm/day}
#'   \item{Period}{Factor column with levels "Warmup", "Calibration", "Validation", or NA}
#' @param plot_point_size Numeric value for point size in the plot (default = 1)
#'
#' @return A ggplot object showing the time series of specific discharge with 
#'         color-coded periods
#'
#' @details
#' The plot includes:
#' - Lines connecting all points
#' - Points colored by period (orange for warmup, red for calibration, blue for validation)
#' - Points with NA periods are not highlighted
#' - Annual x-axis breaks
#' - Rotated x-axis labels for better readability
#' - Legend at the top of the plot
#'
#' @examples
#' q_dly <- data.frame(
#'   date = seq.Date(from = as.Date("2000-01-01"), 
#'                   to = as.Date("2002-12-31"), 
#'                   by = "day"),
#'   Qmm = runif(1096, 0, 10),
#'   Period = factor(c(rep(c("Warmup", "Calibration", "Validation", NA), 
#'                      c(365, 365, 365, 1))))
#' )
#' plot_calib_valid_period(q_dly)
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_point labs theme_bw 
#'             scale_color_manual theme scale_x_date
plot_calib_valid_period <- function(q_dly, plot_point_size = 1) {
  # Input validation
  required_cols <- c("date", "Q_mm_day", "Period")
  if (!all(required_cols %in% names(q_dly))) {
    stop("q_dly must contain columns: ", paste(required_cols, collapse = ", "))
  }
  
  if (!inherits(q_dly$date, "Date")) {
    stop("date column must be of class Date")
  }
  
  if (!is.numeric(q_dly$Q_mm_day)) {
    stop("Qmm column must be numeric")
  }
  
  # Check that non-NA values in Period are valid
  expected_periods <- c("Warmup", "Calibration", "Validation")
  actual_periods <- unique(q_dly$Period[!is.na(q_dly$Period)])
  if (!all(actual_periods %in% expected_periods)) {
    stop("Period column must contain only: ", 
         paste(c(expected_periods, "NA"), collapse = ", "))
  }
  
  # Define color scheme
  period_colors <- c(
    "Warmup" = "orange",
    "Calibration" = "red",
    "Validation" = "steelblue"
  )
  
  # Create plot
  pl <- ggplot(data = q_dly, aes(x = date, y = Q_mm_day, group = 1)) +
    # Add lines
    geom_line(aes(color = Period), linewidth = 0.5) +
    
    # Add points for each period, excluding NA
    geom_point(
      data = subset(q_dly, Period == "Warmup"),
      color = period_colors["Warmup"],
      size = plot_point_size
    ) +
    geom_point(
      data = subset(q_dly, Period == "Calibration"),
      color = period_colors["Calibration"],
      size = plot_point_size
    ) +
    geom_point(
      data = subset(q_dly, Period == "Validation"),
      color = period_colors["Validation"],
      size = plot_point_size
    ) +
    
    # Labels and theme
    labs(
      x = "Date",
      y = "Specific Discharge [mm/day]",
      color = "Period"
    ) +
    theme_bw() +
    scale_color_manual(
      values = period_colors,
      na.value = "grey50"  # Color for NA values in the line
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top"
    ) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  
  return(pl)
}
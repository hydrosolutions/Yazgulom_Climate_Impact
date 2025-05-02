#' Plot Exceedance Curves for Hydrological Data
#'
#' @description
#' Creates an exceedance probability plot for discharge data, comparing 
#' observations with a specified scenario across different time periods.
#'
#' @param data A data frame containing discharge data with columns:
#'   \code{scenario}, \code{period}, \code{probability}, and \code{Q_m3_sec}.
#' @param scen Character string specifying which scenario to compare with observations.
#' @param title_line Character string for the plot title, defaults to empty string.
#'
#' @return A ggplot object showing exceedance curves with discharge values on the y-axis
#'   and exceedance probability (as percentage) on the x-axis.
#'
#' @details
#' The function filters the input data frame to include only observations and the 
#' specified scenario. It creates exceedance curves with a custom color palette that
#' uses dark gray and four shades of blue to distinguish different periods.
#'
#' Exceedance probability is calculated as 100*(1-probability) and displayed as a percentage.
#'
#' @examples
#' \dontrun{
#' # Assuming 'flow_data' has the required columns
#' plot_exceedance(flow_data, "climate_change", "Climate Change Impact on River Flows")
#' }
#'
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal theme scale_color_manual
#'
#' @export
# Function to plot exceedance curves
plot_exceedance <- function(data, scen, title_line = "") {
  
  # Custom palette: dark gray followed by four shades of blue
  custom_palette <- c("#4B4B4B",  # Dark Gray
                      "#0D3B66",  # Dark Blue
                      "#2874A6",  # Medium-Dark Blue
                      "#6497B1",  # Medium-Light Blue
                      "#B3CDE0")  # Pale Pastel Blue
  
  data |> 
    filter(scenario == "observation" | scenario == scen) |> 
    ggplot(aes(x = 100*(1- probability), y = Q_m3_sec, color = period)) +
    geom_line(size = 1.1) +
    labs(title = title_line,
         x = "Probability of Exceedance [%]",
         y = "Discharge [m3/s]",
         color = "Period") +
    theme_minimal() +
    theme(legend.position = "bottom") +
    # use custom palette
    scale_color_manual(values = custom_palette)
}
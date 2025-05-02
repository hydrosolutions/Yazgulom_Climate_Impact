#' Calculate Exceedance Probability for Discharge Data
#'
#' @description
#' Calculates exceedance probability for a data frame containing discharge measurements,
#' arranges values in ascending order, and adds day of year information.
#'
#' @param df A data frame containing discharge data with columns:
#'   \code{Q_m3_sec} (discharge values) and \code{date} (measurement dates).
#'
#' @return A data frame with the original data plus additional columns:
#'   \code{probability} (exceedance probability) and \code{day_of_year} (day of year extracted from date).
#'
#' @details
#' The function first sorts the data frame by discharge values in ascending order,
#' then calculates the probability using the Weibull plotting position formula: rank/(n+1).
#' Finally, it adds a column for the day of year derived from the date.
#'
#' @examples
#' \dontrun{
#' # Assuming 'flow_data' has Q_m3_sec and date columns
#' flow_data_with_exceedance <- calculate_exceedance(flow_data)
#' }
#'
#' @importFrom dplyr arrange mutate
#' @importFrom lubridate yday
#'
#' @export
# Function to calculate exceedance probability
calculate_exceedance <- function(df) {
  df %>%
    arrange(Q_m3_sec) %>%
    mutate(probability = (1:n()) / (n() + 1)) %>%
    mutate(day_of_year = yday(date))
}
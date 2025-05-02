#' Calculate Mean Annual Glacier Runoff
#' 
#' This function processes glacier runoff data to calculate mean annual values
#' and adds scenario information.
#' 
#' @param data A data frame containing glacier runoff data with columns:
#'   - date: Date of measurement
#'   - runoff: Daily runoff values
#' @param scenario_name String identifying the scenario for the data set
#' 
#' @return A data frame with columns:
#'   - year: Year of measurement
#'   - scenario: Scenario identifier
#'   - runoff: Mean annual runoff value
#'
#' @details The function:
#'   1. Adds scenario information to the data
#'   2. Extracts years from dates
#'   3. Calculates mean annual runoff values, ignoring NA values
prepare_glacier_data_mean_annual_runoff <- function(data, scenario_name) {
  # Process data to calculate mean annual runoff
  data |>
    # Add scenario information
    mutate(scenario = scenario_name) |>
    # Extract year from date
    mutate(year = year(date)) |>
    # Group by year and scenario for annual means
    group_by(year, scenario) |>
    # Calculate mean annual runoff
    summarise(
      runoff = mean(runoff, na.rm = TRUE),
      .groups = 'drop'
    )
}
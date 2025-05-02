#' Process Regional Statistical Model (RSM) Data
#' 
#' This function processes RSM data by separating temperature and precipitation
#' measurements, converting them to long format, and adding scenario information.
#' 
#' @param data A data frame containing RSM data with:
#'   - First column named 'Station' containing dates
#'   - First half of remaining columns containing temperature data
#'   - Second half containing precipitation data
#'   - First 8 rows containing metadata
#' @param scenario String identifying the scenario for the data set
#' 
#' @return A data frame in long format with columns:
#'   - date: Date of measurement
#'   - station: Station identifier
#'   - value: Measured value
#'   - sensor: Type of measurement ('T' for temperature, 'P' for precipitation)
#'   - scenario: Scenario identifier
#'
#' @details The function performs these steps:
#'   1. Splits input data into temperature and precipitation sections
#'   2. Processes dates from string format to date objects
#'   3. Converts data from wide to long format
#'   4. Combines temperature and precipitation data
#'   5. Adds scenario information
#'   6. Converts column types appropriately
process_rsm_data <- function(data, scenario) {
  # Process temperature data
  data_T <- data |> 
    # Select date column and temperature columns (first half of data)
    dplyr::select(1:((ncol(data)-1)/2 + 1)) |> 
    # Set column names from first row
    setNames(data[1,]) |> 
    # Remove metadata rows
    slice(-c(1:8)) |> 
    # Rename first column to 'date'
    rename(date = Station) |> 
    # Extract date string
    mutate(date = str_sub(date, start = 1, end = 11)) |> 
    # Convert to date object
    mutate(date = dmy(date))
  
  # Convert temperature data to long format
  data_T_long <- data_T |> 
    pivot_longer(
      cols = -date,
      names_to = "station",
      values_to = "value"
    ) |> 
    mutate(sensor = "T")
  
  # Process precipitation data
  data_P <- data |>
    # Select date column and precipitation columns (second half of data)
    dplyr::select(1, ((ncol(data)-1)/2 + 2):ncol(data)) |> 
    setNames(data[1,]) |> 
    slice(-c(1:8)) |>
    rename(date = Station) |>
    mutate(
      date = str_sub(date, start = 1, end = 11),
      date = dmy(date)
    )
  
  # Convert precipitation data to long format
  data_P_long <- data_P |>
    pivot_longer(
      cols = -date,
      names_to = "station",
      values_to = "value"
    ) |>
    mutate(sensor = "P")
  
  # Combine temperature and precipitation data
  data_long <- bind_rows(data_T_long, data_P_long) |> 
    # Add scenario information
    mutate(scenario = scenario) |>
    # Convert column types appropriately
    type_convert()
  
  return(data_long)
}
#' Process Glacier Runoff Data for RS MINERVE Format
#' 
#' This function processes glacier runoff data into the specific format required
#' by RS MINERVE software. It handles date formatting and adds required metadata
#' rows for each measurement station.
#' 
#' @param glacier_runoff_hist_obs A data frame containing glacier runoff data with:
#'   - date: POSIXct dates in UTC timezone
#'   - Multiple columns starting with "Gl" containing runoff values
#' 
#' @return A data frame formatted for RS MINERVE with:
#'   - First row: Column names
#'   - Rows 2-8: Metadata (X, Y, Z coordinates, sensor type, category, unit, interpolation)
#'   - Remaining rows: Formatted dates and runoff values
#'
#' @details The function performs these steps:
#'   1. Converts dates to RS MINERVE character format
#'   2. Rounds runoff values to 6 decimal places
#'   3. Adds required metadata rows for RS MINERVE compatibility
#'   4. All discharge values are labeled as 'Q' type sensors with 'm3/s' units
process_glacier_runoff <- function(glacier_runoff_hist_obs) {
  # Format dates and process runoff values
  dates_char_Gl <- posixct2rsminerveChar(
    glacier_runoff_hist_obs$date,
    tz = "UTC"
  ) |>
    # Rename date column to match RS MINERVE requirements
    rename(Station = value) |>
    # Add processed runoff columns
    tibble::add_column(
      (glacier_runoff_hist_obs |> 
         # Select only glacier runoff columns
         dplyr::select(starts_with("Gl")) |> 
         # Round values and format as character
         mutate(
           across(starts_with("Gl"),
                  \(x) round(x, digits = 6)),
           across(starts_with("Gl"),
                  \(x) sprintf("%f", x))
         ))
    )
  
  # Create metadata rows required by RS MINERVE
  glaciers_rsm_hist_obs <- rbind(
    # Column headers
    colnames(dates_char_Gl),
    # Coordinates (all set to 0)
    c("X", rep(0, dim(dates_char_Gl)[2] - 1)),
    c("Y", rep(0, dim(dates_char_Gl)[2] - 1)),
    c("Z", rep(0, dim(dates_char_Gl)[2] - 1)),
    # Measurement metadata
    c("Sensor", rep("Q", dim(dates_char_Gl)[2] - 1)),
    c("Category", rep("Flow", dim(dates_char_Gl)[2] - 1)),
    c("Unit", rep("m3/s", dim(dates_char_Gl)[2] - 1)),
    c("Interpolation", rep("Linear", dim(dates_char_Gl)[2] - 1)),
    # Add processed data
    dates_char_Gl
  )
  
  return(glaciers_rsm_hist_obs)
}
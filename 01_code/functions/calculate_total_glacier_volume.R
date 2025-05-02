#' Calculate Total Glacier Volume
#'
#' This function calculates the volume of individual glaciers and the total glacier volume
#' for a basin based on the Erasov method, which relates glacier area to glacier volume.
#'
#' @param rgi_in_basins A dataframe containing glacier data with at least the column 
#'   'glacier_area_m2' representing glacier areas in square meters.
#'
#' @return A dataframe with the original data plus an additional column 'glacier_volume_km3' 
#'   containing the calculated volume for each glacier in cubic kilometers.
#'
#' @details The function converts glacier areas from square meters to square kilometers,
#'   calculates individual glacier volumes using the Erasov method, and adds these volumes
#'   to the input dataframe.
#'
#' @examples
#' \dontrun{
#'   glacier_data <- data.frame(glacier_area_m2 = c(1e6, 2e6, 3e6))
#'   result <- calculate_total_glacier_volume(glacier_data)
#' }
#'
#' @seealso \code{\link{glacierVolume_Erasov}} for the volume calculation method
#'
#' @export
calculate_total_glacier_volume <- function(rgi_in_basins) {
  # Convert glacier areas from m² to km²
  areas_km2 <- rgi_in_basins$glacier_area_m2 / 1e6
  
  # Calculate volume for each glacier using Erasov method
  individual_volumes <- glacierVolume_Erasov(areas_km2)
  
  # Add volumes to the original dataframe
  rgi_in_basins$glacier_volume_km3 <- individual_volumes
  
  # Return the dataframe with added volume column
  return(rgi_in_basins)
}
#' Process Hydrologic Response Unit (HRU) Shapefile for Modeling
#' 
#' This function processes a HRU shapefile to prepare it for hydrological modeling
#' and GCM (Global Climate Model) forcing data extraction. It calculates areas,
#' centroids, and formats specific fields.
#' 
#' @param hru An sf (simple features) object representing Hydrologic Response Units
#'   Should contain at least:
#'   - geometry: spatial polygons
#'   - code: identifier for downstream junction
#' 
#' @return An sf object with additional calculated fields:
#'   - area: area in square meters (formatted)
#'   - area_km2: area in square kilometers (formatted)
#'   - X: X-coordinate of centroid
#'   - Y: Y-coordinate of centroid
#'   - DownJunct: downstream junction identifier (copied from code)
#'
#' @details The function performs the following operations:
#'   1. Calculates area in square meters using sf::st_area
#'   2. Converts area to square kilometers
#'   3. Calculates centroids and extracts their coordinates
#'   4. Formats area fields to specified decimal places
#'   5. Assigns downstream junction identifiers
#'
#' @note Area values are formatted with 2 decimal places for square meters
#'       and 6 decimal places for square kilometers. Both use a field width
#'       of 15 characters for consistent formatting.
post_process_hru <- function(hru) {
  # Process HRU data with area calculations and centroid extraction
  processed_data <- hru |> 
    mutate(
      # Calculate areas and convert units
      area = as.numeric(st_area(geometry)),
      area_km2 = as.numeric(area) / 10^6,
      
      # Calculate centroids and extract coordinates
      centroid = st_centroid(geometry),
      X = st_coordinates(centroid)[, 1],
      Y = st_coordinates(centroid)[, 2], 
      
      # Assign downstream junction identifier
      DownJunct = code
    ) |>
    # Remove temporary centroid column
    dplyr::select(-centroid) |>
    # Format area fields for consistent output
    mutate(
      area = formatC(area, format = "f", digits = 2, width = 15),
      area_km2 = formatC(area_km2, format = "f", digits = 6, width = 15)
    )
  
  return(processed_data)
}
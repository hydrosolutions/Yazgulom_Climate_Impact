#' Extract Year from Filename
#' 
#' This function extracts the first four-digit year from the basename of a file path
#' and optionally validates it against a threshold year.
#' 
#' @param filename A string containing a file path that includes a four-digit year in the basename
#' @param threshold_year A numeric value representing the threshold year for comparison (default = NULL)
#' 
#' @return If threshold_year is NULL, returns the extracted year as a numeric value.
#'         If threshold_year is provided, returns a logical value: TRUE if the extracted year 
#'         is less than the threshold, FALSE otherwise. Throws an error if no valid year is found.
#'
#' @details The function extracts the first four-digit sequence from the basename of the input path.
#'          If no four-digit sequence is found, the function will throw an error.
#'          If a threshold_year is provided, it compares the extracted year with the threshold.
#'
#' @examples
#' extract_year("data_2023.csv")  # Returns 2023
#' extract_year("tas_2019.nc", 2020)  # Returns TRUE
#' extract_year("tas_2020.nc", 2020)  # Returns FALSE
#' extract_year("path/to/CHELSA_tas__1999_55_85_30_50_V.2.1.nc")  # Returns 1999
#' extract_year("nodate.txt")  # Throws an error
#'
#' @export
extract_year <- function(filename, threshold_year = NULL) {
  # Get just the base filename (last component of the path)
  # This ensures we only look at the filename, not the full path
  base_name <- basename(filename)
  
  # Extract the first four-digit sequence from the base filename
  # This will find patterns like '1999' in 'CHELSA_tas__1999_55_85_30_50_V.2.1.nc'
  year_str <- stringr::str_extract(base_name, "\\d{4}")
  
  # Check if a year was found - str_extract returns NA if no match is found
  if (is.na(year_str)) {
    stop("No four-digit year found in the filename.")
  }
  
  # Convert extracted string to numeric for comparison operations
  year_num <- as.numeric(year_str)
  
  # If threshold_year is provided, compare and return logical result
  if (!is.null(threshold_year)) {
    return(year_num < threshold_year)
  }
  
  # Otherwise, return the extracted year as a numeric value
  return(year_num)
}
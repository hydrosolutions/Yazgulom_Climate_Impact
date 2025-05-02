#' Process RS MINERVE Climate Files
#' 
#' This function processes RS MINERVE climate forcing files (hist_obs, hist_sim, or fut_sim)
#' and separates them into temperature and precipitation dataframes for each elevation band.
#' The input file should contain alternating columns of temperature and precipitation data
#' for each elevation band, with the first column containing dates.
#'
#' @param df A dataframe containing RS MINERVE climate data. The first column should contain
#'           dates, followed by alternating columns of temperature and precipitation data
#'           for each elevation band. The first 8 rows contain metadata and are skipped
#'           during processing.
#'
#' @return A list containing three elements:
#'   \item{dates}{A tibble containing the processed dates}
#'   \item{T_bands}{A dataframe containing temperature data for each elevation band (L1, L2, etc.)}
#'   \item{P_bands}{A dataframe containing precipitation data for each elevation band (L1, L2, etc.)}
#'
#' @note The elevation bands are ordered from highest (L1) to lowest elevation (Ln).
#'       Negative precipitation values are set to 0.
#'
#' @examples
#' # Read RS MINERVE climate file
#' hist_obs <- read_csv("hist_obs_rsm.csv", col_names = FALSE)
#' hist_obs <- hist_obs %>% dplyr::select(-ncol(hist_obs))
#' 
#' # Process the data
#' processed_data <- process_rsminerve_climate_files(hist_obs)
#' 
#' # Access components
#' dates <- processed_data$dates
#' T_bands <- processed_data$T_bands
#' P_bands <- processed_data$P_bands
#'
process_rsminerve_climate_files <- function(df) {
  # Calculate number of elevation bands
  n_el_bands <- (ncol(df) - 1) / 2
  n_el_bands_2 <- 2 * n_el_bands
  
  # Extract dates from the first column
  dates_tbl <- df %>% 
    pull(1) %>% 
    tail(-8) %>% 
    dmy_hms() %>% 
    as_tibble()
  
  # Process temperature bands
  hist_obs_T_bands <- df %>% 
    tail(-8) %>% 
    dplyr::select(-1) %>% 
    mutate(across(everything(), as.numeric)) %>%
    dplyr::select(1:n_el_bands) %>%
    dplyr::select(rev(names(.))) %>%
    setNames(paste0("L", 1:n_el_bands))
  
  # Process precipitation bands
  hist_obs_P_bands <- df %>% 
    tail(-8) %>% 
    dplyr::select(-1) %>% 
    mutate(across(everything(), as.numeric)) %>%
    dplyr::select((n_el_bands + 1):n_el_bands_2) %>%
    dplyr::select(rev(names(.))) %>%
    setNames(paste0("L", 1:n_el_bands))
  
  # Handle precipitation values less than 0
  hist_obs_P_bands[hist_obs_P_bands < 0] <- 0
  
  # Return a list containing all processed data
  return(list(
    dates = dates_tbl,
    T_bands = hist_obs_T_bands,
    P_bands = hist_obs_P_bands
  ))
}
#' Process Glacier Melt Data for Multiple Climate Models and Scenarios
#'
#' This function disaggregates monthly glacier melt data to daily values for multiple
#' climate models and scenarios using temperature-based disaggregation. For each
#' model-scenario combination, it uses daily temperature data to distribute monthly
#' melt values across days within each month.
#'
#' @param glacier_data A list containing monthly glacier melt data for each scenario.
#'        Each element should be a data frame with columns:
#'        \itemize{
#'          \item date: dates in Date format
#'          \item Qmm: monthly melt values
#'        }
#' @param forcing_data_processed A list containing temperature data for each model-scenario
#'        combination. Each element should contain:
#'        \itemize{
#'          \item T_bands: matrix of temperature values
#'          \item dates: POSIXlt dates
#'        }
#' @param gcm_Models Character vector of climate model names (e.g., "GFDL-ESM4")
#' @param gcm_Scenarios Character vector of scenario names (e.g., "ssp126")
#'
#' @return A nested list structure containing disaggregated daily melt values:
#'         \itemize{
#'           \item First level: scenarios
#'           \item Second level: models
#'           \item Values: data frames with daily dates and melt values
#'         }
#'
#' @examples
#' \dontrun{
#' gcm_Models <- c("GFDL-ESM4", "IPSL-CM6A-LR", "MRI-ESM2-0", "UKESM1-0-LL")
#' gcm_Scenarios <- c("ssp126", "ssp245", "ssp370", "ssp585")
#'
#' glacier_data_disaggregated <- process_glacier_data(
#'   glacier_data = glacier_data,
#'   forcing_data_processed = forcing_data_processed,
#'   gcm_Models = gcm_Models,
#'   gcm_Scenarios = gcm_Scenarios
#' )
#'
#' # Access results for specific model-scenario combination
#' results_gfdl_ssp126 <- glacier_data_disaggregated[["ssp126"]][["GFDL-ESM4"]]
#' }
#'
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr tibble
process_glacier_data <- function(glacier_data, forcing_data_processed, 
                                 gcm_Models, gcm_Scenarios) {
  
  # Initialize empty list to store results
  glacier_data_disaggregated <- list()
  
  # Loop through each scenario
  for (scenario in gcm_Scenarios) {
    # Create nested list for this scenario
    glacier_data_disaggregated[[scenario]] <- list()
    
    # Get the monthly melt data for this scenario and rename column
    monthly_melt <- glacier_data[[scenario]]
    names(monthly_melt)[names(monthly_melt) == "Q_mm_day"] <- "melt"  # Rename Qmm to melt
    monthly_melt <- monthly_melt %>% mutate(date = as.Date(date))
    
    # Loop through each model
    for (model in gcm_Models) {
      # Construct the key for forcing_data_processed
      #key <- paste0(model, "_", scenario)
      key <- model
      
      # Extract temperature data for this model
      if (!is.null(forcing_data_processed[[key]]$T_bands)) {
        # Calculate mean temperature for this model
        temp_series <- rowMeans(forcing_data_processed[[key]]$T_bands, 
                                na.rm = TRUE)
        
        # Create daily temperature dataframe
        posix_dates <- forcing_data_processed[[key]]$dates
        daily_temp <- tibble(
          date = as.Date(format(posix_dates$value, "%Y-%m-%d")),
          temp = temp_series
        )
        
        # Run disaggregation for this model
        disaggregated <- disaggregate_glacier_melt(
          monthly_melt = monthly_melt,
          daily_temp = daily_temp,
          method = "temperature"
        )
        
        # Store results with model name
        glacier_data_disaggregated[[scenario]][[model]] <- disaggregated
      }
    }
  }
  
  return(glacier_data_disaggregated)
}
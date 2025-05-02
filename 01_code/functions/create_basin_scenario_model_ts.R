#' Create Basin Time Series Data
#'
#' @param forcing_data_processed List of processed forcing data for each model-scenario combination
#' @param glacier_data_disaggregated List of disaggregated glacier melt data
#' @return Nested list of dataframes with basin time series data
#' @importFrom lubridate yday
create_basin_scenario_model_ts <- function(forcing_data_processed, glacier_data_disaggregated) {
  # Initialize empty list for results
  basin_fut_ts <- list()
  
  # Get scenarios and models from glacier data
  scenarios <- names(glacier_data_disaggregated)
  
  for (scenario in scenarios) {
    basin_fut_ts[[scenario]] <- list()
    models <- names(glacier_data_disaggregated[[scenario]])
    
    for (model in models) {
      # Get the key for forcing data
      key <- paste0(model, "_", scenario)
      
      # Extract data from forcing_data_processed
      dates <- as.Date(forcing_data_processed[[key]]$dates$value)
      temp_data <- rowMeans(forcing_data_processed[[key]]$T_bands, na.rm = TRUE)
      precip_data <- rowSums(forcing_data_processed[[key]]$P_bands, na.rm = TRUE)
      
      # Get melt data
      melt_data <- glacier_data_disaggregated[[scenario]][[model]] 
      
      # Calculate Julian day
      jour_jul <- yday(dates)
      
      # Calculate PET using airGR::PE_Oudin. The lat is from the basin_centroid variable.
      pet <- airGR::PE_Oudin(JD = jour_jul, Temp = temp_data, LatUnit = "deg", Lat = 39.17838)
      
      # Create dataframe
      basin_df <- data.frame(
        date = dates,
        JourJul = jour_jul,
        Ptot = precip_data,
        Temp = temp_data,
        PET = pet,
        melt = melt_data$melt
      )
      
      # Store in nested list
      basin_fut_ts[[scenario]][[model]] <- basin_df
    }
  }
  
  return(basin_fut_ts)
}
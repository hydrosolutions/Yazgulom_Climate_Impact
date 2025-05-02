#' Process Climate Model Inputs for Multiple Scenarios
#'
#' @description
#' Creates a list of processed model inputs for different climate scenarios and models
#' by applying the airGR model setup and calculating solid precipitation fractions for
#' each elevation layer. The function processes temperature and precipitation data
#' across multiple elevation bands and prepares them for hydrological modeling.
#'
#' @param basin_fut_ts A nested list containing basin time series data:
#'   \itemize{
#'     \item First level: climate scenarios (e.g., "ssp126", "ssp245")
#'     \item Second level: climate models (e.g., "GFDL-ESM4", "IPSL-CM6A-LR")
#'     \item Each model contains a dataframe with columns:
#'       \itemize{
#'         \item date: POSIXct dates
#'         \item Ptot: total precipitation
#'         \item Temp: mean temperature
#'         \item PET: potential evapotranspiration
#'       }
#'   }
#' @param Basin_Info A list containing basin characteristics:
#'   \itemize{
#'     \item HypsoData: hypsometric data for the basin
#'     \item ZLayers: elevation layers information
#'   }
#' @param fun_model The airGR model function to be used for creating inputs
#'
#' @return A list where each entry represents a scenario-model combination (e.g., "ssp126_GFDL-ESM4")
#'         containing the processed airGR model inputs with:
#'   \itemize{
#'     \item Standard airGR input variables
#'     \item LayerTempMean: temperature data for each elevation layer
#'     \item LayerPrecip: precipitation data for each elevation layer
#'     \item LayerFracSolidPrecip: calculated solid precipitation fraction for each layer
#'   }
#'
#' @examples
#' \dontrun{
#' # Process climate scenarios
#' climate_models <- process_inputs_models_scenarios(
#'   basin_fut_ts = basin_fut_ts,
#'   Basin_Info = Basin_Info,
#'   fun_model = airGR::CreateRunModel
#' )
#'
#' # Access specific scenario-model combination
#' ssp126_gfdl <- climate_models[["ssp126_GFDL-ESM4"]]
#' }
#'
#' @importFrom airGR CreateInputsModel
process_inputs_models_scenarios <- function(basin_fut_ts, Basin_Info, fun_model) {
  # Initialize results list
  climate_models <- list()
  
  # Get scenarios and models
  scenarios <- names(basin_fut_ts)
  
  for (scenario in scenarios) {
    models <- names(basin_fut_ts[[scenario]])
    
    for (model in models) {
      # Get current scenario-model data
      current_ts <- basin_fut_ts[[scenario]][[model]]
      
      # Create model inputs
      inputsModel <- airGR::CreateInputsModel(
        FUN_MOD = fun_model,
        DatesR = current_ts$date |> as.POSIXct(),
        Precip = current_ts$Ptot,
        PotEvap = current_ts$PET,
        TempMean = current_ts$Temp,
        HypsoData = Basin_Info$HypsoData,
        ZInputs = median(Basin_Info$HypsoData),
        NLayers = length(Basin_Info$ZLayers),
        verbose = FALSE
      )
      
      # Get key for forcing data
      key <- paste0(model, "_", scenario)
      
      # Extract and process temperature and precipitation bands
      T_bands <- forcing_data_processed[[key]]$T_bands
      P_bands <- forcing_data_processed[[key]]$P_bands
      
      # Modify inputs with our data
      inputsModel$LayerTempMean <- as.list(T_bands)
      inputsModel$LayerPrecip <- as.list(P_bands)
      
      # Compute solid precipitation fraction
      solid_frac_pr <- solid_fraction_elevation_layer(
        Basin_Info,
        inputsModel$LayerTempMean,
        tas_min = 8.56,
        tas_max = 9.97
      )
      inputsModel$LayerFracSolidPrecip <- solid_frac_pr
      
      # Store in results list
      #model_key <- paste(scenario, model, sep = "_")
      climate_models[[scenario]][[model]] <- inputsModel
      #climate_models[[model_key]] <- inputsModel
    }
  }
  
  return(climate_models)
}
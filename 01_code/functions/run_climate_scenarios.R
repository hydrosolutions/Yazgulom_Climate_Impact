#' Run Climate Change Scenario Simulations with Snow Reset
#'
#' This function executes hydrological model simulations for multiple climate scenarios
#' and models, with periodic resetting of snow accumulation to prevent unrealistic
#' buildup in high elevation bands.
#'
#' @param scenarios Character vector of climate scenarios (e.g., "ssp126", "ssp585")
#' @param models Character vector of climate models (e.g., "GFDL-ESM4", "IPSL-CM6A-LR")
#' @param inputs_models_scenarios Nested list of processed model inputs for each scenario-model combination
#' @param clim_scen_start Start date of simulation (Date object)
#' @param clim_scen_end End date of simulation (Date object)
#' @param model_2_use String specifying the model type ("GR4J", "GR5J", or "GR6J")
#' @param param Model parameters from calibration
#' @param initial_states Initial model states from validation period
#' @param reset_years Integer specifying interval for snow reset (default = 5)
#'
#' @return Nested list structure:
#'   - First level: scenarios
#'   - Second level: models
#'   - Third level: model outputs including:
#'     - Time series of flows and states
#'     - CemaNeige snow module outputs
#'     - Model states and parameters
#'
#' @details The function:
#'   1. Splits simulation into chunks of specified length (reset_years)
#'   2. Runs the model for each chunk
#'   3. Caps snow accumulation at 1000mm for each layer
#'   4. Accumulates results across chunks while preserving all model outputs
#'
#' @note Snow capping prevents unrealistic accumulation in high elevation bands
#'       during long-term climate simulations while maintaining water balance.
run_climate_scenarios <- function(scenarios, models, inputs_models_scenarios, 
                                  clim_scen_start, clim_scen_end,
                                  model_2_use, param, initial_states,
                                  reset_years = 5, reset_level = 0, reset_threshold = 1000) {
  # Initialize results list
  model_runs <- list()
  
  # Calculate simulation period
  total_years <- interval(clim_scen_start, clim_scen_end) %>% 
    time_length(unit = "years") %>% 
    floor()
  
  # Create sequence of reset dates
  reset_dates <- seq(from = clim_scen_start,
                     to = clim_scen_end,
                     by = paste(reset_years, "years"))
  
  # Loop through scenarios
  for (scenario in scenarios) {
    model_runs[[scenario]] <- list()
    
    # Loop through models
    for (model in models) {
      print(paste("Running model for scenario:", scenario, "and model:", model))
      inputsModel_clim_run <- inputs_models_scenarios[[scenario]][[model]]
      accumulated_results <- NULL
      current_states <- initial_states
      
      # Run model in chunks with snow reset
      for (i in 1:(length(reset_dates)-1)) {
        # Define chunk period
        chunk_start <- reset_dates[i]
        chunk_end <- reset_dates[i+1] - days(1)
        
        # Get indices for current chunk
        indRun_clim_run <- which(inputsModel_clim_run$DatesR >= chunk_start & 
                                   inputsModel_clim_run$DatesR <= chunk_end)
        
        # Create run options
        runOptions_clim_run <- airGR::CreateRunOptions(
          FUN_MOD = fun_model,
          InputsModel = inputsModel_clim_run,
          IndPeriod_Run = indRun_clim_run,
          IniStates = current_states,
          IniResLevels = NULL,
          IndPeriod_WarmUp = 0L
        )
        
        # Run appropriate model version
        chunk_results <- switch(model_2_use,
                                "GR4J" = RunModel_CemaNeigeGR4J(InputsModel = inputsModel_clim_run,
                                                                RunOptions = runOptions_clim_run,
                                                                Param = param),
                                "GR5J" = RunModel_CemaNeigeGR5J(InputsModel = inputsModel_clim_run,
                                                                RunOptions = runOptions_clim_run,
                                                                Param = param),
                                "GR6J" = RunModel_CemaNeigeGR6J(InputsModel = inputsModel_clim_run,
                                                                RunOptions = runOptions_clim_run,
                                                                Param = param)
        )
        
        # Update states and cap snow accumulation
        current_states <- chunk_results$StateEnd
        current_states$CemaNeigeLayers <- lapply(current_states$CemaNeigeLayers, 
                                                 function(x) {
                                                   x[x > reset_threshold] <- reset_level
                                                   return(x)
                                                 })
        
        # Initialize or update accumulated results
        if (is.null(accumulated_results)) {
          accumulated_results <- chunk_results
        } else {
          # Accumulate standard model outputs
          numeric_outputs <- c("PotEvap", "Precip", "Prod", "Pn", "Ps", "AE", "Perc",
                               "PR", "Q9", "Q1", "Rout", "Exch", "AExch1", "AExch2",
                               "AExch", "QR", "QD", "Qsim")
          
          for (output in numeric_outputs) {
            accumulated_results[[output]] <- c(accumulated_results[[output]], 
                                               chunk_results[[output]])
          }
          
          # Update dates
          accumulated_results$DatesR <- c(accumulated_results$DatesR, chunk_results$DatesR)
          
          # Accumulate snow module outputs
          layer_vars <- c("Pliq", "Psol", "SnowPack", "ThermalState", "Gratio", 
                          "PotMelt", "Melt", "PliqAndMelt", "Temp", "Gthreshold", 
                          "Glocalmax")
          
          # Update each layer's variables
          for (layer_num in 1:length(accumulated_results$CemaNeigeLayers)) {
            for (var in layer_vars) {
              accumulated_results$CemaNeigeLayers[[layer_num]][[var]] <- 
                c(accumulated_results$CemaNeigeLayers[[layer_num]][[var]],
                  chunk_results$CemaNeigeLayers[[layer_num]][[var]])
            }
          }
          
          # Update model state tracking
          accumulated_results$RunOptions$IndPeriod_Run <- 
            c(accumulated_results$RunOptions$IndPeriod_Run, indRun_clim_run)
          accumulated_results$StateEnd <- chunk_results$StateEnd
        }
      }
      
      # Store final results
      model_runs[[scenario]][[model]] <- accumulated_results
    }
  }
  
  return(model_runs)
}
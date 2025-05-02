# Function to run model with given parameters
run_model_with_params <- function(param) {
  # Run model with the parameters
  if (model_2_use == "GR4J") {
    runResults <- airGR::RunModel_CemaNeigeGR4J(
      InputsModel = inputsModel,
      RunOptions = runOptions_cal,
      Param = param
    )
  } else if (model_2_use == "GR5J") {
    runResults <- airGR::RunModel_CemaNeigeGR5J(
      InputsModel = inputsModel,
      RunOptions = runOptions_cal,
      Param = param
    )
  } else if (model_2_use == "GR6J") {
    runResults <- airGR::RunModel_CemaNeigeGR6J(
      InputsModel = inputsModel,
      RunOptions = runOptions_cal,
      Param = param
    )
  }
  
  return(runResults)
}
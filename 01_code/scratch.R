# This script is a modification of the original script to implement a monthly calibration approach.
## Tobias Siegfried, hydrosolutions GmbH
## April 29, 2025

# LIBRARIES ----
library(pacman)
p_load(tidyverse, airGR)

# DATA ----
path = here::here("01_code", "config_param.R")
source(path)
forcing_q_ts <- read_csv(file.path(config$paths$model_dir, "forcing_q_ts.csv"))
basin_obs_ts <- forcing_q_ts |> rename(Q_mm_day_no_glacier = Q_mm_day)

# Basin information
Basin_Info <- read_rds(file.path(config$paths$data_path, "Basin_Info.rds"))
Basin_Info |> summary()

# time periods and calibration/validation periods
cal_val_per <- readRDS(file.path(config$paths$data_path, "Calibration_Validation_Period.rds"))
cal_val_per |> summary()

# Snow reset level and threshold. This is the level at which the snow is reset in the model at the end of a simulation period chunk. This is important to avoid consistent snow accumulation in high elevation catchments. All is in mm.
snow_reset_level <- 0 # mm
snow_reset_threshold <- 1000 # mm

# warmup period
n_warmup_years <- 3

# FUNCTIONS ----
# Custom function to aggregate daily model outputs to monthly for comparison with observations
ErrorCrit_NSE_Monthly <- function(InputsCrit, OutputsModel) {
  # Extract the simulated daily discharge
  Qsim_daily <- OutputsModel$Qsim
  
  # Extract the observation dates from the model run
  obs_dates <- InputsCrit$InputsModel$DatesR[InputsCrit$RunOptions$IndPeriod_Run]
  
  # Create a data frame with dates and simulated values
  df_sim <- data.frame(
    date = obs_dates,
    Qsim = Qsim_daily
  )
  
  # Aggregate to monthly mean (first day of month)
  df_sim <- df_sim %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(Qsim_monthly = mean(Qsim, na.rm = TRUE))
  
  # Extract the observed discharge (already monthly)
  Qobs <- InputsCrit$Obs
  obs_dates <- InputsCrit$InputsModel$DatesR[InputsCrit$RunOptions$IndPeriod_Run]
  
  # Create a data frame with dates and observed values
  df_obs <- data.frame(
    date = obs_dates,
    Qobs = Qobs
  )
  
  # Aggregate to monthly (should already be monthly, but ensuring consistent format)
  df_obs <- df_obs %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(Qobs_monthly = mean(Qobs, na.rm = TRUE))
  
  # Join simulated and observed monthly values
  df_combined <- df_sim %>%
    inner_join(df_obs, by = "month")
  
  # Calculate NSE on the monthly values
  Qobs_monthly <- df_combined$Qobs_monthly
  Qsim_monthly <- df_combined$Qsim_monthly
  
  # Apply transformation if specified
  if(!is.null(InputsCrit$transfo)) {
    if(InputsCrit$transfo == "sqrt") {
      Qobs_monthly <- sqrt(Qobs_monthly)
      Qsim_monthly <- sqrt(Qsim_monthly)
    } else if(InputsCrit$transfo == "log") {
      epsilon <- min(Qobs_monthly[Qobs_monthly > 0], na.rm = TRUE) / 100
      Qobs_monthly <- log(Qobs_monthly + epsilon)
      Qsim_monthly <- log(Qsim_monthly + epsilon)
    }
  }
  
  # Calculate NSE
  if(all(is.na(Qobs_monthly)) || all(is.na(Qsim_monthly))) {
    NSE <- NA
  } else {
    moy_Qobs <- mean(Qobs_monthly, na.rm = TRUE)
    NSE <- 1 - sum((Qsim_monthly - Qobs_monthly)^2, na.rm = TRUE) / 
      sum((Qobs_monthly - moy_Qobs)^2, na.rm = TRUE)
  }
  
  # Apply weights if provided
  if(!is.null(InputsCrit$Weights)) {
    NSE <- NSE * InputsCrit$Weights
  }
  
  # Store and return results
  OutputsCrit <- list()
  OutputsCrit$CritValue <- -NSE  # Negative because calibration minimizes
  OutputsCrit$CritName <- "NSE_Monthly"
  OutputsCrit$CritBestValue <- -1 # Negative because of minimization
  
  return(OutputsCrit)
}

# Custom modification of main script for monthly calibration
# Replace your existing calibration setup with this

# 1. First prepare data to ensure monthly observations are correctly aligned
prepare_monthly_calibration <- function(basin_obs_ts, cal_val_per, n_warmup_years, fun_model) {
  # Get monthly dates for observation data
  monthly_dates <- basin_obs_ts %>%
    mutate(month = floor_date(date, "month")) %>%
    distinct(month) %>%
    pull(month)
  
  # Find calibration period indexes based on monthly aggregation
  indRun_cal_monthly <- which(monthly_dates >= cal_val_per$calib_start & 
                                monthly_dates <= cal_val_per$calib_end)
  
  # Find validation period indexes 
  indRun_val_monthly <- which(monthly_dates >= cal_val_per$valid_start & 
                                monthly_dates <= cal_val_per$valid_end)
  
  # Find daily indices for calibration
  indRun_cal <- which(basin_obs_ts$date >= cal_val_per$calib_start & 
                        basin_obs_ts$date <= cal_val_per$calib_end)
  
  # Find daily indices for validation  
  indRun_val <- which(basin_obs_ts$date >= cal_val_per$valid_start & 
                        basin_obs_ts$date <= cal_val_per$valid_end)
  
  # Determine warmup period
  warmup_period_ind <- 1:(365*n_warmup_years + 1)
  
  # Adjust calibration period for warmup
  indRun_cal <- indRun_cal + length(warmup_period_ind)
  indRun_cal_max <- max(indRun_cal)
  indRun_cal <- indRun_cal[indRun_cal <= indRun_cal_max]
  
  # Get monthly observation data for calibration and validation
  q_obs_monthly <- basin_obs_ts %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(Q_mm_day = mean(Q_mm_day_no_glacier, na.rm = TRUE)) %>%
    pull(Q_mm_day)
  
  # Extract the monthly observations for calibration and validation periods
  q_obs_cal <- q_obs_monthly[indRun_cal_monthly]
  q_obs_val <- q_obs_monthly[indRun_val_monthly]
  
  return(list(
    indRun_cal = indRun_cal,
    indRun_val = indRun_val,
    warmup_period_ind = warmup_period_ind,
    q_obs_cal = q_obs_cal,
    q_obs_val = q_obs_val
  ))
}

# Example usage in your main script:
# -----------------------------
# Modify your existing code to use the monthly calibration approach

# Monthly calibration setup
cal_setup <- prepare_monthly_calibration(basin_obs_ts, cal_val_per, n_warmup_years, fun_model)

# MODEL ----

# Prepare model inputs as before
inputsModel <- airGR::CreateInputsModel(FUN_MOD            = fun_model, 
                                        DatesR             = basin_obs_ts$date,
                                        Precip             = basin_obs_ts$Ptot, 
                                        PotEvap            = basin_obs_ts$PET,
                                        TempMean           = basin_obs_ts$Temp, 
                                        HypsoData          = Basin_Info$HypsoData,
                                        ZInputs            = median(Basin_Info$HypsoData),
                                        NLayers            = length(Basin_Info$ZLayers))

# Apply your custom adjustments to inputsModel as before
inputsModel_ours <- inputsModel
inputsModel_ours$LayerTempMean <- hist_obs_T_bands |> as.list() 
inputsModel_ours$LayerPrecip <- hist_obs_P_bands |> as.list()
solid_frac_pr <- solid_fraction_elevation_layer(Basin_Info,
                                                inputsModel_ours$LayerTempMean, 
                                                tas_min  = 8.56, 
                                                tas_max  = 9.97)
inputsModel_ours$LayerFracSolidPrecip <- solid_frac_pr
inputsModel <- inputsModel_ours

# Create run options for calibration
runOptions_cal <- airGR::CreateRunOptions(FUN_MOD          = fun_model,
                                          InputsModel      = inputsModel,
                                          IndPeriod_Run    = cal_setup$indRun_cal,
                                          IniStates        = NULL,
                                          IniResLevels     = NULL,
                                          IndPeriod_WarmUp = cal_setup$warmup_period_ind,
                                          IsHyst           = FALSE,
                                          warnings         = FALSE)

# Create inputs for criteria calculation using our custom function
inputsCrit_cal <- list(
  FUN_CRIT = ErrorCrit_NSE_Monthly,  # Use our custom monthly NSE function
  InputsModel = inputsModel,
  RunOptions = runOptions_cal,
  transfo = "",                      # or "sqrt", "log" as needed
  Obs = cal_setup$q_obs_cal,         # Monthly observations
  Weights = c(1)
)
class(inputsCrit_cal) <- "InputsCrit"

# Create calibration options
calibOptions <- airGR::CreateCalibOptions(FUN_MOD = fun_model, 
                                          FUN_CALIB = airGR::Calibration_Michel)

# Run calibration with custom objective function
outputsCalib <- airGR::Calibration_Michel(
  InputsModel = inputsModel,
  RunOptions = runOptions_cal,
  InputsCrit = inputsCrit_cal,
  CalibOptions = calibOptions,
  FUN_MOD = fun_model
)

# Get calibrated parameters
param <- outputsCalib$ParamFinalR

# Run the model with calibrated parameters
if (model_2_use == "GR4J") {
  runResults_cal <- airGR::RunModel_CemaNeigeGR4J(InputsModel = inputsModel,
                                                  RunOptions = runOptions_cal,
                                                  Param = param)
} else if (model_2_use == "GR5J") {
  runResults_cal <- airGR::RunModel_CemaNeigeGR5J(InputsModel = inputsModel,
                                                  RunOptions = runOptions_cal,
                                                  Param = param)
} else if (model_2_use == "GR6J") {
  runResults_cal <- airGR::RunModel_CemaNeigeGR6J(InputsModel = inputsModel,
                                                  RunOptions = runOptions_cal,
                                                  Param = param)
}

# Calculate performance using our custom function
OutputsCrit <- ErrorCrit_NSE_Monthly(InputsCrit = inputsCrit_cal, OutputsModel = runResults_cal)
cat("Calibration NSE (monthly):", -OutputsCrit$CritValue, "\n")

# Plotting function for monthly comparison
plot_monthly_comparison <- function(runResults, obs_data, dates, period_name) {
  # Extract simulated values
  Qsim_daily <- runResults$Qsim
  
  # Create data frame with dates and simulated values
  df_sim <- data.frame(
    date = dates,
    Qsim = Qsim_daily
  )
  
  # Aggregate to monthly
  df_sim_monthly <- df_sim %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(Qsim_monthly = mean(Qsim, na.rm = TRUE))
  
  # Create data frame with dates and observed values
  df_obs <- data.frame(
    date = dates,
    Qobs = obs_data
  )
  
  # Aggregate to monthly
  df_obs_monthly <- df_obs %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month) %>%
    summarize(Qobs_monthly = mean(Qobs, na.rm = TRUE))
  
  # Join simulated and observed
  df_combined <- df_sim_monthly %>%
    full_join(df_obs_monthly, by = "month")
  
  # Plot
  p <- ggplot(df_combined) +
    geom_line(aes(x = month, y = Qsim_monthly, color = "Simulated")) +
    geom_point(aes(x = month, y = Qsim_monthly, color = "Simulated")) +
    geom_line(aes(x = month, y = Qobs_monthly, color = "Observed")) +
    geom_point(aes(x = month, y = Qobs_monthly, color = "Observed")) +
    scale_color_manual(values = c("Simulated" = "blue", "Observed" = "red")) +
    labs(title = paste("Monthly Comparison -", period_name),
         x = "Date", y = "Discharge (mm/day)", color = "") +
    theme_minimal()
  
  return(p)
}

# Create a monthly comparison plot for calibration period
cal_dates <- basin_obs_ts$date[cal_setup$indRun_cal]
cal_plot <- plot_monthly_comparison(runResults_cal, cal_setup$q_obs_cal, cal_dates, "Calibration")
print(cal_plot)

# Validation section would follow similar approach
# [Validation code here...]


##########






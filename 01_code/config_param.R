# CONFIGURATION AND PARAMETERS ----
# Centralized settings for Atbashy 2025 River Basin climate impact modeling
#
#
# This file contains all configuration settings and parameters used throughout
# the hydrological modeling study. Centralizing these values makes the
# analysis more reproducible, maintainable, and adaptable to other river basins.
#
# CONFIGURATION SECTIONS:
#
# PROJECT METADATA:
#   - River and basin identification
#   - Gauge information
#   - Study period definitions
#
# FILE PATHS:
#   - Data directory locations
#   - Input/output paths
#   - GIS and climate data repositories
#
# MODELING PARAMETERS:
#   - Time periods (historical, future, calibration, validation)
#   - Climate models and scenarios
#   - Hydrological modeling timesteps
#   - Processing options and thresholds
#
# GIS PARAMETERS:
#   - Coordinate reference systems
#   - Elevation band parameters
#   - Stream extraction thresholds
#
# VISUALIZATION SETTINGS:
#   - Color palettes for different data types
#   - Plot styling parameters
#   - Standard figure dimensions
#
# USAGE NOTES:
# - Modify these parameters to adapt the workflow to different river basins
# - Derived parameters are calculated automatically based on base settings
# - Date sequences are generated for consistent time periods
#
# STRUCTURE:
# The configuration is stored as a nested list structure that can be accessed as:
#   config$section$parameter
#
# AUTHORS:
#   - Original settings: Tobias Siegfried, Aidar Zhumabaev
#   - Configuration structure: Tobias Siegfried and Claude 3.7
#
# LAST UPDATED: 2025-03-03, Tobias Siegfried, hydrosolutions GmbH
#
# CHANGE LOG
#   - 2025-04-27: Update script with small changes to emphasize the overall study goal
#                 which is to produce a hydrological discharge time series for 2017 - 2023.

# Load required packages for parameter processing ----
p_load(lubridate, tidyverse, here)

# Load generate_seq_dates function ----
source(here::here("01_code", "functions","generateSeqDates.R"))

# Project configuration ----
config <- list(
  # Project metadata
  project = list(
    river_name = "Yazgulom",
    basin_name = "Pyandzh",
    gauge_code = "17068",
    gauge_name = "Montravi"
  ),

  #File paths
  paths = list(
    data_path = here::here("02_data"),
    gis_path = here::here("02_data", "GIS"),
    hist_obs_dir = here::here("02_data", "CHELSA_V21"),
    hist_sim_dir = here::here("02_data", "Central_Asia_Domain", "CMIP6", "OBS_SIM"),
    fut_sim_dir = here::here("02_data", "Central_Asia_Domain", "CMIP6", "FUT_SIM"),
    model_dir = here::here("05_models"),
    glacier_path = here::here("02_data", "Glaciers"),
    figures_path = here::here("06_figures")
  ),

  # Time periods
  periods = list(
    # Historical observations
    hist_obs_start = 1979,
    hist_obs_end = 1993,

    # Historical GCM simulations (same as observations)
    hist_sim_start = 1979,
    hist_sim_end = 1993,

    # Future simulations - here we adjust to the period of interest for Hanna's paper
    fut_sim_start = 2011,
    fut_sim_end = 2099,

    # Calibration and validation
    calib_start = "1979-01-01",
    calib_end = "1988-12-31",
    valid_start = "1989-01-01",
    valid_end = "1993-12-31"
  ),

  # Models and scenarios
  # Note: below, I add fut_sim_models and hist_sim_models
  climate = list(
    models = c("GFDL-ESM4", "IPSL-CM6A-LR", "MRI-ESM2-0", "UKESM1-0-LL"),
    hist_sim_models = "ERA5", #c("GFDL-ESM4", "IPSL-CM6A-LR", "MRI-ESM2-0", "UKESM1-0-LL"),
    fut_sim_models = "ERA5", #c("GFDL-ESM4", "IPSL-CM6A-LR", "MRI-ESM2-0", "UKESM1-0-LL"),
    scenarios = c("ssp126", "ssp245", "ssp370", "ssp585"),
    hydrology_run_scenarios = "run",
    obs_freq = "day"
  ),

  # GIS parameters
  gis = list(
    utm42n = "EPSG:32642",
    crs_project = 4326,  # WGS84
    dem_file = "dem.tif",
    basin_shape_file = "Atbaschy_Atschakomandi_Basin.gpkg",
    gauge_shape_file = "Atbaschy_Atschakomandi_Gauge.gpkg"
  ),

  # Elevation band parameters
  hru = list(
    dh_elband = 500  # Elevation interval
  ),

  # Color palettes
  colors = list(

    # Set scenario colors and labels
    scenario_colors = c(
      "ssp126" = "#2196F3",  # Light Blue
      "ssp245" = "#4CAF50",  # Light Green
      "ssp370" = "#FFA000",  # Light Orange
      "ssp585" = "#F44336"   # Light Red
    ),

    scenario_palette = c(
      "ssp126" = "#2196F3",  # Light Blue
      "ssp245" = "#4CAF50",  # Light Green
      "ssp370" = "#FFA000",  # Light Orange
      "ssp585" = "#F44336"   # Light Red
    ),

    scenario_labels = c(
      "ssp126" = "SSP1-2.6",
      "ssp245" = "SSP2-4.5",
      "ssp370" = "SSP3-7.0",
      "ssp585" = "SSP5-8.5"
    ),

    scenario_colors_with_obs = c(
      "ssp126" = "#2196F3",  # Light Blue
      "ssp245" = "#4CAF50",  # Light Green
      "ssp370" = "#FFA000",  # Light Orange
      "ssp585" = "#F44336",  # Light Red
      "Observed" = "black"   # Black
    ),

    scenario_labels_with_obs = c(
      "ssp126" = "SSP1-2.6",
      "ssp245" = "SSP2-4.5",
      "ssp370" = "SSP3-7.0",
      "ssp585" = "SSP5-8.5",
      "Observed" = "Observed"
    ),

    # Elevation color palette for hypsometric tinting
    elevation_colors = c(
      "#13543E", "#26744C", "#498C4B", "#86B955", "#C8CF93",
      "#DAD7B4", "#E7DAC3", "#F1E3CF", "#F5EBE1", "#FFFFFF"
    ),

    # HRU colors
    atbashy_colors = c(
      "Atbashy_1"  = "#F4A8A8",  # Salmon pink
      "Atbashy_2"  = "#E69138",  # Orange
      "Atbashy_3"  = "#B5B13B",  # Olive green
      "Atbashy_4"  = "#8FBE73",  # Light green
      "Atbashy_5"  = "#76A988",  # Sea green
      "Atbashy_6"  = "#5EA799",  # Teal
      "Atbashy_7"  = "#4AB5C9",  # Light blue
      "Atbashy_8"  = "#4A88C9",  # Medium blue
      "Atbashy_9"  = "#3D64B9",  # Dark blue
      "Atbashy_10" = "#9B4AB9", # Purple
      "Atbashy_11" = "#C44AB9", # Pink
      "Atbashy_12" = "#E198B4"  # Light pink
    )
  ),

  # Processing options
  options = list(
    rescale_pr = "N",  # Whether to rescale precipitation
    streams_threshold = 30000,  # Threshold for stream extraction
    hydro_model_time_steps = "mon",  # Time steps for hydrological modeling (day, dec, or mon)
    sec_month = 30.4375 * 24 * 3600  # seconds per month
  )
)

# Generate derived configurations ----
config$climate$model_scenarios <- expand.grid(
  config$climate$models, config$climate$scenarios) |>
  mutate(combination = paste0(Var1, "_", Var2)) |>
  pull(combination)

config$climate$hydrology_run_model_scenarios <- expand.grid(
  config$climate$hist_sim_models, config$climate$hydrology_run_scenarios) |>
  mutate(combination = paste0(Var1, "_", Var2)) |>
  pull(combination)

# Generate date sequences ----
config$dates <- list(
  hist_obs_dates = generateSeqDates(
    config$periods$hist_obs_start,
    config$periods$hist_obs_end,
    config$climate$obs_freq
  )$date |>
    as_date() |>
    as_tibble() |>
    rename(Date = value),

  fut_sim_dates = generateSeqDates(
    config$periods$fut_sim_start,
    config$periods$fut_sim_end,
    config$climate$obs_freq
  )$date |>
    as_date() |>
    as_tibble() |>
    rename(Date = value)
)
config$dates$hist_sim_dates <- config$dates$hist_obs_dates

#' Extract hydrological response unit HRU specific climate time series from
#' nc-files.
#'
#' Function extracts precipitation (tp) or temperature (tas_) data from climate
#' raster bricks (observed history obs_hist, GCM-simulated history hist_sim and
#' GCM-simulated future (fut_sim) and prepares a dataframe for later import in
#' RSMinerve (use readr::write_csv(.,col_names=FALSE)). tp and tas_ have to be
#' exported, the function has to be called twice and the resulting tibble
#' columns added.
#'
#' @param climate_files List of either temperature or precipitation climate .nc
#'   files to process (do not mix!). Make sure the file list time interval is
#'   consistent with startY and endY.
#' @param catchmentName Name of catchment for which data should be extracted
#' @param temp_or_precip Either 'Temperature' or 'Precipitation'
#' @param elBands_shp Shapefile with hydrological response units. The column
#'   containing the names of the hydrological response units must be \code{name}
#'   and the column containing the average elevation of the elevation band must
#'   be \code{Z}. Please make sure that the shape file is in UTM coordinates.
#' @param startY Starting year for which data should be made available (assuming
#'   data 'is' available from the start of that year)
#' @param endY Ending year from which data should be extracted (assuming data
#'   'is' actually available until the end of that year)
#' @param obs_frequency Climate observation frequency ('hour', 'day', 'month')
#' @param climate_data_type String of denoting observation type. Either
#'   'hist_obs' (historical observations, i.e. CHELSA V21 high resolution
#'   climate data), 'hist_sim' (GCM model output data over the historical
#'   period) and 'fut_sim' (fture GCM simulations)
#' @param crs_in_use Proj code for crs in use. For example
#'   '+proj=longlat +datum=WGS84' for epsg 4326
#' @param output_file_dir Path to output file dir (if empty, file will not be
#'   written)
#' @param tz Time zone information. Default "UTC" which can be overridden.
#' @return Dataframe tibble with temperature in deg. C. or precipitation in mm/h
#' @family Pre-processing
#' @note This function currently can only read input files with full years of
#'   data, that is, with data from January to December of a given year.
#' @export

gen_HRU_Climate_CSV_RSMinerve_local <- function(climate_files,
                                                catchmentName,
                                                temp_or_precip,
                                                elBands_shp,
                                                startY,
                                                endY,
                                                obs_frequency,
                                                climate_data_type,
                                                crs_in_use,
                                                output_file_dir = 0,
                                                gcm_model = 0,
                                                gcm_scenario = 0,
                                                tz = "UTC") {
  # Transform shapefile CRS to match the input CRS
  elBands_shp_latlon <- sf::st_transform(elBands_shp, crs = sf::st_crs(crs_in_use))
  crs_elBands <- sf::st_crs(elBands_shp)
  
  # Generate date sequence
  dateElBands <- generateSeqDates(startY, endY, obs_frequency, tz)
  datesChar <- posixct2rsminerveChar(dateElBands$date, tz) %>%
    dplyr::rename(Station = value)
  
  # Get names of elevation bands
  namesElBands <- elBands_shp$name
  dataElBands_df <- purrr::map_dfc(namesElBands, ~ stats::setNames(list(logical()), .x))
  
  # .nc-file extraction using terra and exactextractr
  for (climate_file in climate_files) {
    message("Processing File: ", climate_file)
    histobs_data <- terra::rast(climate_file)
    
    # == in case the raster is not properly georeferenced - this is an ongoing Baustelle!
    # load nc-file
    #nc <- ncdf4::nc_open(climate_files_tas[[1]])
    #lon <- ncdf4::ncvar_get(nc, "longitude")
    #lat <- ncdf4::ncvar_get(nc, "latitude")
    #nc_close(nc)
    # Flip the latitude dimension of the data
    #f <- terra::flip(histobs_data, direction = "vertical")
    # Set the extent
    #ext <- terra::ext(min(lon), max(lon), min(lat), max(lat))
    #terra::ext(histobs_data) <- ext
    # Set the CRS
    #crs <- "EPSG:4326"
    #terra::crs(histobs_data) <- crs
    # ==

    subbasin_data <- 
      exactextractr::exact_extract(histobs_data, elBands_shp_latlon, 'mean', progress = FALSE) %>%
      t() %>%
      tibble::as_tibble(.name_repair = "unique") %>%
      dplyr::slice(1:nrow(dateElBands));
    
    names(subbasin_data) <- names(dataElBands_df)
    dataElBands_df <- tibble::add_row(dataElBands_df, subbasin_data)
  }
  
  dataElBands_df_data <- cbind(datesChar, dataElBands_df) %>%
    tibble::as_tibble(.name_repair = "unique")
  
  # Construct CSV file header
  header <- tibble::tibble(Station = c('X', 'Y', 'Z', 'Sensor', 'Category', 'Unit', 'Interpolation'))
  
  elBands_XY <- sf::st_transform(elBands_shp, crs = crs_elBands) %>%
    sf::st_centroid() %>%
    sf::st_coordinates() %>%
    tibble::as_tibble(.name_repair = "unique")
  
  elBands_Z <- elBands_shp$Z %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::rename(Z = value)
  
  elBands_XYZ <- cbind(elBands_XY, elBands_Z) %>%
    as.matrix() %>%
    t() %>%
    tibble::as_tibble(.name_repair = "unique") %>%
    dplyr::mutate_all(as.character)
  
  names(elBands_XYZ) <- names(dataElBands_df)
  
  # Sensor (P or T), Category, Unit and Interpolation
  nBands <- ncol(elBands_XYZ)
  sensorType <- rep(ifelse(temp_or_precip == 'Temperature', 'T', 'P'), nBands)
  unit <- rep(ifelse(temp_or_precip == 'Temperature', 'C', 'mm/d'), nBands)
  category <- rep(temp_or_precip, nBands)
  interpolation <- rep('Linear', nBands)
  sensor <- tibble::as_tibble(rbind(sensorType, category, unit, interpolation), 
                              .name_repair = "unique")
  names(sensor) <- names(dataElBands_df)
  
  # Combine all data
  file2write <- bind_rows(elBands_XYZ, sensor)
  file2write <- bind_cols(header, file2write)
  file2write <- bind_rows(file2write, dataElBands_df_data %>%
                            dplyr::mutate_all(as.character))
  
  # Write file to disk
  if (output_file_dir != 0) {
    file_name <- paste0(output_file_dir, climate_data_type, "_", 
                        ifelse(gcm_model != 0, paste0(gcm_model, "_"), ""), 
                        ifelse(gcm_scenario != 0, paste0(gcm_scenario, "_"), ""), 
                        temp_or_precip, "_", startY, "_", endY, ".csv")
    readr::write_csv(file2write, file_name, col_names = FALSE)
  }
  
  return(file2write)
}
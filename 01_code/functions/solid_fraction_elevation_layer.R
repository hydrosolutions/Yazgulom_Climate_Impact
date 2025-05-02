#' Calculate Solid Precipitation Fraction for Elevation Bands
#' 
#' This function computes the fraction of solid precipitation for each elevation band
#' using the Glazirin method and USACE formula. The function is adapted from the 
#' DataAltiExtrapolation_valery() function in the airGR package.
#'
#' @param Basin_Info A list containing:
#'   \item{ZLayers}{Numeric vector of elevation band heights in meters}
#'
#' @param tas Numeric list or vector of mean air temperatures for each elevation band
#' @param tas_min Numeric scalar of minimum temperature threshold for solid precipitation
#' @param tas_max Numeric scalar of maximum temperature threshold for solid precipitation
#'
#' @return A named list where each element corresponds to an elevation band (L1, L2, etc.)
#'         containing the fraction of solid precipitation (0 to 1) for that band.
#'         L1 represents the highest elevation band.
#'
#' @details
#' The function uses two main steps:
#' 1. Calculates temperature thresholds using the Glazirin formula for each elevation band:
#'    threshold = 1.25 + 0.016 * z + 0.207 * z^2 (where z is elevation in km)
#' 2. Applies the USACE formula to compute solid fraction between the min and max thresholds
#'
#' @examples
#' Basin_Info <- list(ZLayers = c(4000, 3500, 3000))
#' tas <- list(0, 2, 4)  # Mean temperatures for each band
#' result <- solid_fraction_elevation_layer(Basin_Info, tas, -1, 3)
#'
#' @references
#' Glazirin, G. E. (1997). Precipitation distribution with altitude.
#' Theoretical and Applied Climatology, 58(3-4), 141-145.
#'
solid_fraction_elevation_layer <- function(Basin_Info, tas, tas_min, tas_max) {
  # Input validation
  if (!is.list(Basin_Info) || is.null(Basin_Info$ZLayers)) {
    stop("Basin_Info must be a list containing ZLayers")
  }
  
  if (!is.numeric(tas_min) || !is.numeric(tas_max)) {
    stop("tas_min and tas_max must be numeric")
  }
  
  if (tas_min >= tas_max) {
    stop("tas_min must be less than tas_max")
  }
  
  # Convert elevation to kilometers for Glazirin formula
  z_km <- Basin_Info$ZLayers / 1000
  
  # Calculate number of layers
  NLayers <- length(Basin_Info$ZLayers)
  
  # Convert tas to list if it's a vector
  if (!is.list(tas)) {
    if (length(tas) != NLayers) {
      stop("Length of tas must match number of elevation bands")
    }
    tas <- as.list(tas)
  }
  
  # Calculate Glazirin threshold temperatures
  tas_threshold_glazirin <- 1.25 + 0.016 * z_km + 0.207 * z_km^2
  
  # Initialize output list
  LayerFracSolidPrecip <- vector("list", NLayers)
  names(LayerFracSolidPrecip) <- paste0("L", 1:NLayers)
  
  # Calculate solid fraction for each layer
  for (iLayer in 1:NLayers) {
    # Calculate band-specific temperature thresholds
    tas_min_band <- tas_min + tas_threshold_glazirin[iLayer]
    tas_max_band <- tas_max + tas_threshold_glazirin[iLayer]
    
    # Get mean temperature for current band
    TempMean_band <- tas[[iLayer]]
    
    # Calculate solid fraction using USACE formula
    SolidFraction <- 1 - (TempMean_band - tas_min_band) / (tas_max_band - tas_min_band)
    
    # Apply boundary conditions
    SolidFraction[TempMean_band > tas_max_band] <- 0
    SolidFraction[TempMean_band < tas_min_band] <- 1
    
    # Store results
    LayerFracSolidPrecip[[iLayer]] <- as.double(SolidFraction)
  }
  
  return(LayerFracSolidPrecip)
}
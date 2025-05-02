#' Calculates glacier volume from glacier area.
#'
#' Empirical scaling function to calculate glacier volume based on glacier area
#' by Erasov, 1968. Only works for positive glacier area.
#' @param area_km2 Glacier area in km2
#' @return Glacier volume in km3
#' @export
#' @family Glacier functions
#' @seealso \code{\link{glacierArea_Erasov}}, \code{\link{glacierVolume_RGIF}}
glacierVolume_Erasov <- function(area_km2) {
  # Input validation
  if (!is.numeric(area_km2)) {
    stop("Input 'area_km2' must be numeric")
  }
  
  volume_km3 <- ifelse(
    area_km2 <= 0,
    0,
    0.027 * area_km2^(1.5)
  )
  
  return(volume_km3)
}
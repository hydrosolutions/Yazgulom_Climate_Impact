#' Calculate and Plot Hypsometric Curve from DEM
#' 
#' This function calculates the hypsometric curve, hypsometric integral, and related
#' statistics from a Digital Elevation Model (DEM). It can produce both absolute
#' and relative height curves.
#' 
#' @param dem_raster A terra SpatRaster object containing elevation data
#' @param relative_height Logical indicating whether to plot relative heights (TRUE)
#'        or absolute elevations (FALSE)
#' @param plot Logical indicating whether to display the plot
#' 
#' @return A list containing:
#'   - curve_data: Data frame with relative area and elevation/height values
#'   - hypsometric_integral: Calculated hypsometric integral
#'   - elevation_stats: List with min, max, and range of elevations
#'   - plot: ggplot object of the hypsometric curve
#'
#' @details The hypsometric curve shows the distribution of elevation with respect
#'          to area within a drainage basin. The hypsometric integral (HI) is
#'          calculated as the area under the relative height curve divided by
#'          total possible area (1.0).
#'
#' @import terra dplyr ggplot2
calculate_hypsometric_curve <- function(dem_raster, 
                                        relative_height = TRUE, 
                                        plot = TRUE) {
  # Extract elevation values from raster
  elev_values <- as.data.frame(terra::values(dem_raster))
  colnames(elev_values) <- "elevation"
  
  # Remove NA values
  elev_values <- na.omit(elev_values)
  
  # Create sequence of area percentiles
  area_percentiles <- seq(0, 100, by = 1)
  
  # Calculate elevation quantiles
  elevation_quantiles <- quantile(
    elev_values$elevation,
    probs = rev(area_percentiles/100),
    na.rm = TRUE
  )
  
  # Create hypsometric data frame
  hyps_data <- data.frame(
    rel_area = area_percentiles,
    elevation = as.numeric(elevation_quantiles)
  ) |>
    mutate(
      rel_height = ((elevation - min(elevation)) / 
                      (max(elevation) - min(elevation))) * 100
    )
  
  # Calculate hypsometric integral
  hi <- sum(hyps_data$rel_height/100) / nrow(hyps_data)
  
  # Set y-axis variable and label based on relative_height parameter
  y_var <- if(relative_height) hyps_data$rel_height else hyps_data$elevation
  y_lab <- if(relative_height) "Relative Height (%)" else "Elevation (m)"
  
  # Create hypsometric curve plot
  p <- ggplot(hyps_data, aes(x = rel_area, y = y_var)) +
    geom_line(color = "blue", size = 1) +
    labs(
      title = "Hypsometric Curve",
      subtitle = if(!relative_height) 
        paste("Elevation range:", 
              round(min(hyps_data$elevation), 1),
              "to",
              round(max(hyps_data$elevation), 1),
              "m") else NULL,
      x = "Relative Area (%)",
      y = y_lab,
      caption = paste("Hypsometric Integral:", round(hi, 3))
    ) +
    theme_minimal() +
    scale_x_continuous(limits = c(0, 100)) +
    if(relative_height) {
      scale_y_continuous(limits = c(0, 100))
    } else {
      scale_y_continuous(expand = expansion(mult = 0.05))
    }
  
  # Add fixed aspect ratio for relative height plots
  if(relative_height) {
    p <- p + coord_fixed(ratio = 1)
  }
  
  # Display plot if requested
  if(plot) {
    print(p)
  }
  
  # Return results
  return(list(
    curve_data = hyps_data,
    hypsometric_integral = hi,
    elevation_stats = list(
      min = min(hyps_data$elevation),
      max = max(hyps_data$elevation),
      range = max(hyps_data$elevation) - min(hyps_data$elevation)
    ),
    plot = p
  ))
}
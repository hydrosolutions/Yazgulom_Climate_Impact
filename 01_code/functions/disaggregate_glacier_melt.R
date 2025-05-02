#' Disaggregate Monthly Glacier Melt to Daily Values
#' 
#' This function converts monthly glacier melt values to daily values using either
#' temperature-based weighting or linear interpolation.
#'
#' @param monthly_melt A data frame containing:
#'   \item{date}{Date column representing the first day of each month}
#'   \item{melt}{Numeric column with monthly melt values in mm/day}
#' @param daily_temp A data frame containing:
#'   \item{date}{Date column with daily timestamps}
#'   \item{temp}{Numeric column with daily temperature values}
#' @param temp_offset_param Numeric value added to temperatures to ensure positive values
#'        for weighting (default = 1)
#' @param weights_scale Scaling factor for temperature weights in exponential function
#'        (default = 10)
#' @param method Character string specifying the disaggregation method:
#'        "temperature" (default) or "linear"
#'
#' @return A tibble with daily dates and corresponding melt values
#'
#' @details
#' The function provides two methods for disaggregation:
#' 
#' 1. Temperature-based method ("temperature"):
#'    - Uses daily temperatures to weight the distribution of monthly melt
#'    - Applies exponential weighting to give more influence to warmer days
#'    - Preserves monthly totals
#'    
#' 2. Linear interpolation method ("linear"):
#'    - Interpolates between mid-month values
#'    - Adjusts results to preserve monthly means
#'    - Handles missing monthly values by leaving interpolated values unadjusted
#'
#' @examples
#' # Create sample monthly data
#' monthly_data <- tibble(
#'   date = seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "month"),
#'   melt = runif(12, 0, 10)
#' )
#' 
#' # Create sample daily temperature data
#' daily_data <- tibble(
#'   date = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day"),
#'   temp = rnorm(366, mean = 10, sd = 5)
#' )
#' 
#' # Disaggregate using temperature-based method
#' daily_melt <- disaggregate_glacier_melt(monthly_data, daily_data)
#'
#' @importFrom lubridate days_in_month year month ymd ceiling_date days
#' @importFrom dplyr filter mutate group_by group_modify ungroup select pull
#' @importFrom tidyr tibble
disaggregate_glacier_melt <- function(monthly_melt, daily_temp, 
                                      temp_offset_param = 1, weights_scale = 10,
                                      method = c("temperature", "linear")) {
  # Match method argument
  method <- match.arg(method)
  
  # Input validation
  if (!all(c("date", "melt") %in% names(monthly_melt))) {
    stop("monthly_melt must contain 'date' and 'melt' columns")
  }
  if (!all(c("date", "temp") %in% names(daily_temp))) {
    stop("daily_temp must contain 'date' and 'temp' columns")
  }
  if (!inherits(monthly_melt$date, "Date") || !inherits(daily_temp$date, "Date")) {
    stop("date columns must be of class Date")
  }
  if (!is.numeric(monthly_melt$melt) || !is.numeric(daily_temp$temp)) {
    stop("melt and temp columns must be numeric")
  }
  if (!is.numeric(temp_offset_param) || !is.numeric(weights_scale)) {
    stop("temp_offset_param and weights_scale must be numeric")
  }
  
  if (method == "linear") {
    # Linear interpolation method
    mid_month_data <- monthly_melt %>%
      mutate(mid_month_date = date)
    
    # Create sequence of dates
    all_dates <- seq(
      from = min(daily_temp$date),
      to = max(daily_temp$date),
      by = "day"
    )
    
    # Convert to relative days for interpolation
    reference_date <- min(all_dates)
    relative_mid_month_days <- as.numeric(
      difftime(mid_month_data$mid_month_date, reference_date, units = "days")
    )
    relative_all_days <- as.numeric(
      difftime(all_dates, reference_date, units = "days")
    )
    
    # Interpolate values
    interpolated_melt <- approx(
      x = relative_mid_month_days,
      y = mid_month_data$melt,
      xout = relative_all_days,
      method = "linear",
      rule = 2
    )
    
    # Create and adjust daily values
    daily_data <- tibble(
      date = all_dates,
      melt = interpolated_melt$y
    ) %>%
      mutate(
        year = year(date),
        month = month(date)
      ) %>%
      group_by(year, month) %>%
      group_modify(function(month_data, group_keys) {
        # Find target monthly value
        target_month_date <- ymd(paste(group_keys$year, group_keys$month, "01"))
        target_monthly_melt <- monthly_melt %>%
          filter(
            year(date) == group_keys$year,
            month(date) == group_keys$month
          ) %>%
          pull(melt)
        
        # Adjust values if monthly value exists
        if (length(target_monthly_melt) > 0) {
          current_mean <- mean(month_data$melt)
          adjustment_factor <- if(current_mean != 0) {
            target_monthly_melt / current_mean
          } else {
            1
          }
          month_data$melt <- month_data$melt * adjustment_factor
        }
        
        month_data
      }) %>%
      ungroup() %>%
      select(date, melt)
    
    return(daily_data)
    
  } else {
    # Temperature-based method
    monthly_melt %>%
      group_by(year = year(date), month = month(date)) %>%
      group_map(function(month_data, group_keys) {
        # Get month dates
        month_date <- ymd(paste(group_keys$year, group_keys$month, "01"))
        month_end <- ceiling_date(month_date, "month") - days(1)
        
        # Get temperature data
        month_temps <- daily_temp %>%
          filter(between(date, month_date, month_end))
        
        # Calculate weights
        temp_offset <- abs(min(month_temps$temp)) + temp_offset_param
        weights <- exp((month_temps$temp + temp_offset) / weights_scale)
        norm_weights <- weights / sum(weights)
        
        # Calculate daily melt
        daily_melt <- norm_weights * (month_data$melt[1] * days_in_month(month_date))
        
        tibble(
          date = month_temps$date,
          melt = daily_melt
        )
      }) %>%
      bind_rows()
  }
}
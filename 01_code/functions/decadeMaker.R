#' Creates a vector of decadal (10 days) dates
#'
#' This function creates a decadal 10-days dates vector that allows date tagging
#' for decadal (10-days) time series. This type of timeseries is usually used for
#' hydro-meteorological data in the former Soviet Republics. The intra-months decades
#' can be configured as dates at the beginning, middle or end of the decade.
#'
#' @param s starting date in YYYY-mm-dd format
#' @param e end date in YYYY-mm-dd format
#' @param type 'start' creates starting decade dates, 'end' creates ending
#'   decade dates, 'mid' or 'middle creates dates in the middle of a decade.
#' @return A sequence of decadal dates
#' @family Helper functions
#' @export
decadeMaker <- function(s, e, type) {
  s_date <- as.Date(s)
  e_date <- as.Date(e)
  s_year <- as.Date(format(s_date, "%Y-01-01"))
  e_year <- as.Date(format(e_date, "%Y-12-31"))
  
  ydiff <- as.numeric(format(e_year, "%Y")) - as.numeric(format(s_year, "%Y")) + 1
  decade <- rep(1:36, times = ydiff)
  
  temp_dates <- seq.Date(s_year, by = "10 days", length.out = length(decade))
  
  eom <- sapply(seq.Date(s_year, by = "month", length.out = ydiff * 12), function(x) {
    as.numeric(format(as.Date(format(x, "%Y-%m-01")) + months(1) - days(1), "%d"))
  })
  
  if (type == 'end') {
    daysV <- rep(c(10, 20, eom), times = ydiff)
  } else if (type == 'start') {
    daysV <- rep(c(1, 11, 21), times = ydiff)
  } else if (type == 'mid' || type == 'middle') {
    daysV <- rep(c(5, 15, 25), times = ydiff)
  }
  
  temp_dates <- temp_dates + daysV - 1
  temp_dates <- temp_dates[temp_dates <= e_date & temp_dates >= s_date]
  
  data.frame(date = temp_dates, dec = decade[1:length(temp_dates)])
}
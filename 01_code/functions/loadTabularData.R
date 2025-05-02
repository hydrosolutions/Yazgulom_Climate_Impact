#' Load Tabular Hydro-Meteorological Data
#'
#' Loads csv files with tabular hydrometeorological data where years are in rows
#' and decades or months are in columns. The function automatically detects if
#' monthly or decadal (10-day) data is provided. The function automatically computes
#' the long-term norm of the data provided and returns a time aware tibble with
#' date, data, data norm columns and code columns.
#'
#' @param fPath Path to the input file
#' @param fName File name (.csv file as input required)
#' @param code Hydrometeorological station code
#' @param stationName Hydrometeorological station name/location
#' @param rName Name of river
#' @param rBasin Name of basin
#' @param dataType Type of data, either `Q` (discharge data), `T` (temperature data) or `P` (precipitation data)
#' @param units Data units
#' @return Time-aware tibble with relevant data
#' @details Note that the input file needs to be in coma-separated format and
#'   without header. \cr
#' \cr
#' The most common format for hydrological data in Central Asia is in tabular form with the years in rows and the months or decades in columns, i.e. the data has 13 columns and as many rows as years of data. An input file containing monthly data might look like follows: \cr
#'   1990,0.1,0.1,0.2,0.2,0.3,0.3,0.4,0.4,0.3,0.2,0.1,0.1 \cr
#'   1991,0.1,0.1,0.2,0.2,0.3,0.3,0.4,0.4,0.3,0.2,0.1,0.1 \cr
#'   1992,0.1,0.1,0.2,0.2,0.3,0.3,0.4,0.4,0.3,0.2,0.1,0.1 \cr
#'   ... \cr
#' An input file containing decadal data (every 10 days) would have 37 columns, the first for the year and the following for each decade in a year. \cr
#' @family Pre-processing
#' @examples
#' \dontrun{
#' demo_data <- loadTabularData(
#'   fPath = "./",
#'   fName = "discharge.csv",
#'   code = "ABC",
#'   stationName = "DemoStation",
#'   rName = "Demo River",
#'   rBasin = "Demo Basin",
#'   dataType = "Q",
#'   unit = "m3/s")
#' }
#' @export
loadTabularData <- function(fPath, fName, code, stationName, rName, rBasin, dataType, units) {
  # Read CSV file using base R
  dataMat <- read.csv(paste(fPath, fName, sep = "/"), header = FALSE)
  
  # Check dimensions and set type
  if (ncol(dataMat) == 13) {
    type <- 'mon'
  } else if (ncol(dataMat) == 37) {
    type <- 'dec'
  } else {
    cat("ERROR please verify that input file has number of columns 13 for monthly data or 37 for decadal data.")
    return(NULL)
  }
  
  # Extract start and end years
  yS <- dataMat[1, 1]
  yE <- dataMat[nrow(dataMat), 1]
  
  # Remove year column and process data
  dataMat <- dataMat[, -1]
  norm <- rep(colMeans(dataMat, na.rm = TRUE), each = nrow(dataMat))
  data <- as.vector(t(dataMat))
  
  # Create date strings
  s <- paste(as.character(yS), "-01-01", sep = "")
  e <- paste(as.character(yE), "-12-31", sep = "")
  
  # Generate dates based on type
  if (type == 'dec') {
    dates <- decadeMaker(s, e, 'end')
    dates <- dates[, -which(names(dates) == "dec"), drop = FALSE]
  } else {
    dates <- monDateSeq(s, e, 12)
    dates <- data.frame(date = dates)
  }
  
  # Create result data frame
  result <- data.frame(
    date = dates$date,
    data = data,
    norm = norm,
    units = units,
    type = dataType,
    code = as.character(code),
    station = stationName,
    river = rName,
    basin = rBasin,
    resolution = factor(type, levels = c("dec", "mon"))
  )
  
  return(result)
}
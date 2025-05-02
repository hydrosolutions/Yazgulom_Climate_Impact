#' Create a monthly sequence date vector with end of month day dates
#'
#' This function creates a sequence of monthly dates between a start and an end
#' date. It requires the specification of the start and end dates and the frequency.
#' If a continuous sequence is requested, then freq should be 12.
#'
#' @param st character string representing the start date in "YYYY-MM-DD" format
#' @param en character string representing the end date in "YYYY-MM-DD" format
#' @param freq integer value specifying the frequency of dates per year (12 for continuous monthly dates)
#' @return A vector of Date objects representing the last day of each month in the sequence
#' @examples
#' monDateSeq("2020-01-01", "2020-12-31", 12)
#' @family Helper functions
#' @export
monDateSeq <- function(st, en, freq) {
    # Convert to Date objects
    start_date <- as.Date(st)
    end_date <- as.Date(en)
    
    # Get sequence of months
    months_seq <- seq(as.POSIXlt(start_date), as.POSIXlt(end_date), 
                     by = paste(as.character(12/freq), "months"))
    
    # Function to get last day of month
    last_day_of_month <- function(date) {
        date <- as.POSIXlt(date)
        # Move to first day of next month and subtract one day
        date$mon <- date$mon + 1
        date$mday <- 1
        as.Date(date) - 1
    }
    
    # Convert to end of month dates
    result <- as.Date(sapply(months_seq, last_day_of_month))
    
    return(result)
}
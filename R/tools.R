#' getage
#' @name getage
#' @author Antoine Fabri and Tjebo Heeren
#' @description calculates age in years, as durations or periods
#' @param from_date start date
#' @param to_date end date
#' @param period Calculating period (TRUE) or duration (FALSE- default)
#' @param dec How many decimals are displayed
#' @return Numeric vector
#' @family convenience functions
#' @import lubridate
#' @seealso
#' [OP on stackoverflow](https://stackoverflow.com/a/47529507/7941188)
#' from which this function was inspired.
#' [Read about periods and durations](
#' https://lubridate.tidyverse.org/articles/lubridate.html#time-intervals)
#' @examples
#' getage("1984-10-16")
#'
#' dob <-  c("1984-10-16", "2000-01-01")
#' test_date <-  as.Date(dob) + c(15000, 20000)
#' getage(dob, test_date)
#' @export
#'
getage <- function(from_date,
                to_date = lubridate::now(),
                period = FALSE,
                dec = 1) {
  if (is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if (is.character(to_date)) to_date <- lubridate::as_date(to_date)
  if (period) {
    age <- lubridate::year(
      lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))
    round(age, dec)
  } else {
    age <- lubridate::interval(
      start = from_date, end = to_date) / lubridate::dyears(1)
    round(age, dec)
  }
}

#' Capitalize words
#' @name tocapital
#' @description capitalises single words
#' @param x string vector
#' @return Character vector
#' @keywords internal
#'
tocapital <- function(x) {
  paste(toupper(substring(x, 1,1)), substring(x, 2),
        sep="", collapse=" ")
}

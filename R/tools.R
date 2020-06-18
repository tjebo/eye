#' age
#' @name age
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
#' age("1984-10-16")
#'
#' dob <-  c("1984-10-16", "2000-01-01")
#' test_date <-  as.Date(dob) + c(15000, 20000)
#' age(dob, test_date)
#' @export
#'
age <- function(from_date,
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

#' csv
#' @description wrapper around write.csv with default 'row.names = FALSE'.
#' Will use the name of the data frame for the generated .csv file.
#' @name csv
#' @param x data frame
#' @param name Filename (Default: Name of dataframe). If provided,
#'  character string (.csv extension added automatically)
#' @family convenience functions
#' @return No return value, called for side effects (saving file)
#' @examples
#' \dontrun{
#' csv(amd)
#' }
#' @export
csv <- function(x, name = NULL) {
  if(is.null(name)){
    name <- deparse(substitute(x))
  }
  file = paste0(name, '.csv')
  utils::write.csv(x, file, row.names = F)
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

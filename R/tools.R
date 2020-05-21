

#' age
#' @name age
#' @description calculates age in years, either as duration or as period
#' Slight modification of a function [posted on stackoverflow by user Moody_Mudskipper](https://stackoverflow.com/a/47529507/7941188)
#' @param from_date start date
#' @param to_date end date
#' @param period default FALSE: output as a duration. If TRUE, output as a period
#' @family convenience functions
#' @examples
#' age("1984-10-16")
#'
#' dob <-  c("1984-10-16", "2000-01-01")
#' test_date <-  as.Date(dob) + c(15000, 20000)
#' age(dob, test_date)
#' @export

age <- function(from_date, to_date = lubridate::now(), period = FALSE, dec = 1){
  if(!require('lubridate'))
    stop('Please install the lubridate package')
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (period) { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))
  round(age, dec)
  } else { age <- lubridate::interval(start = from_date, end = to_date)/lubridate::dyears(1)
  round(age, dec)
  }
}


#' insight
#' @name insight
#' @description Shows commonly used summary statistics
#' @param x either vector or list of vectors. Data frames supported.
#' Character vectors (or character columns) will be removed.
#' @param dec how many decimals are displayed
#' @param dataframe returning list if FALSE
#' @importFrom purrr partial
#' @family stats functions
#' @return data frame (default) or list
#' @examples
#' x = y = z = c(rnorm(20), NA)
#'
#' # named or unnamed list
#' mylist <- list(x = x, y = y, z = z)
#' insight(mylist)
#' # with a data frame
#' mydf <- data.frame(x, y, z)
#' insight(mydf)
#' #If aggregation by group, split the data frame first
#' mydf2 <- data.frame(group = rep(letters[1:2], each = 42), x, y, z)
#' lapply(split(mydf2, mydf2$group), see, rownames = FALSE)
#' @export

insight <- function(x, dec = 1, dataframe = TRUE) {
  funs <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE),
    n = length,
    median = function(x) median(x, na.rm = TRUE),
    min = function(x) min(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE)
  )
  if (is.atomic(x)) {
    list_res <- lapply(funs, function(f) round(f(x), dec))

  } else if (typeof(x) == "list") {
    x_num <- Filter(is.numeric, x)
    if (!identical(x, x_num)) {
      warning("Character columns or list elements removed", call. = FALSE)
    }
    result <- lapply(funs, mapply, x_num)
    list_res <- lapply(result, function(y) round(y, digits = dec))
  }
  if (!dataframe) {
    list_res
  } else {
    data.frame(list_res)
  }
}

#' csv
#' @description wrapper around write.csv with default 'row.names = FALSE'.
#' Will use the name of the data frame for the generated .csv file.
#' @name csv
#' @param x data frame
#' @param name Filename. Default: Name of dataframe to save as csv.
#' Or character string (.csv extension added automatically)
#' @family convenience functions
#' @examples
#' \dontrun{
#' csv(amd)
#' }
#' @export

csv <- function(x, name = deparse(substitute(x))) {
  file = paste0(name, '.csv')
  write.csv(x, file, row.names = F)
}


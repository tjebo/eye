#' eye
#' @name eye
#' @author Tjebo Heeren
#' @description See more with *eye*
#' @section Ophthalmology functions:
#'   *eye* is dedicated to facilitate very common tasks
#'   in ophthalmic research.
#'
#' - Visual acuity conversion for snellen, logMAR and ETDRS [`va`]
#' - Counting patients and eyes [`eyes`]
#' - Recode eye strings [`recodeye`]
#' - Reshape eye specific variables [`myop`] and [`hyperop`]
#' - Summarizing data with common statistics (mean, sd, n, range)[`reveal`]
#' - Easy summary of your eye data in one blink [`blink`]
#'
#'  *eye* contains a real life data set [`eye::amd`]
#' @section Beyond ophthalmology:
#' - [`geom_trail`]: A base plot type = "b" equivalent for the
#'   ggplot2 package
#' - [`age`]: Calculate age
#' - [`csv`]: Conveniently save a data frame to csv
#' @docType package
#' @keywords internal
#' @seealso
#' Useful links:
#' - \href{https://github.com/tjebo/eye}{`eye` on github}
#' - \href{https://github.com/tjebo/eye/issues}{Report bugs}
#' - \href{https://ggplot2.tidyverse.org/}{the ggplot2 package}
utils::globalVariables(c("va_chart"))


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
#'  *eye* contains the real life data sets [`eye::amd`] and [`eye::amd2`]
#' @section Beyond ophthalmology:
#' - [`age`]: Calculate age
#' @docType package
#' @keywords internal
#' @seealso
#' Useful links:
#' - \href{https://github.com/tjebo/eye}{`eye` on github}
#' - \href{https://github.com/tjebo/eye/issues}{Report bugs}
utils::globalVariables(c("va_chart"))


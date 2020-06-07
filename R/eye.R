#' eye
#' @name eye
#' @author Tjebo Heeren
#' @description Analysis of eye data
#' @section Ophthalmology functions:
#' **`eye`** is dedicated to facilitate ophthalmic research with its high
#' level functions
#' - [`va`]: Visual acuity conversion
#' - [`eyes`]: Count eyes and patients
#' - [`myop`]: Make eye data "long"
#' - [`blink`]: wrapper around `va`, `myop` and `eyes`
#'  *eye* contains a real life data set [`eye::amd`] and some functions beyond ophtalmology, which could make your data analysis a tiny bit more convenient.
#' @section Beyond ophthalmology:
#' - [`reveal`]: Get common summary statistics
#' - [`geom_trail`]: A base plot type = "b" equivalent for [ggplot2::ggplot2]
#' - [`age`]: Calculate age
#' - [`csv`]: Conveniently save a data frame to csv
#' @docType package
#' @seealso
#' Useful links:
#' - \href{https://github.com/tjebo/eye}{`eye` on github}
#' - \href{https://github.com/tjebo/eye/issues}{Report bugs}
utils::globalVariables(c("va_chart", "funs_eyeReveal"))


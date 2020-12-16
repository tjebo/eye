#' Plausibility checking
#' @name plausibility_methods
#' @description S3 methods for checking plausibility of VA entries
#' @param x vector of visual acuities
#' @param ... further arguments passed to methods
#' @section Plausibility checks:
#' VA can be snellen (fraction: feet/meter), snellen decimal, logMAR, ETDRS, or
#' "qualitative" (Counting fingers, etc.)
#'
#'   - Snellen fractions need to be either form 6/x or 20/x
#'   - Snellen decimal must be >= 0 and <= 2
#'   - ETDRS must be between 0 and 100
#'   - logMAR must be between -0.3 and 3.0
#'   - Qualitative must be PL, LP, NLP, NPL, HM, CF (any case allowed)
#'
#' Any element which is not recognized will be converted to NA
#' @section VA classes:
#' convert_VA returns a vector of three classes:
#'   1. `va`
#'   1. One of `snellen`, `logmar`, `etdrs` or `quali`.
#'   1. Either of `character` (for Snellen and qualitative),
#'       `numeric` (for logMAR), or `integer` (for ETDRS).
#' @return vector with visual acuity of class `va`. See also "VA classes"
#' @family VA converter
#' @export
checkVA <- function (x, ...) {
  UseMethod("checkVA", x)
}

#' @rdname plausibility_methods
#' @export
checkVA.quali <- function(x, ...){
  test <- is.na(x) | x %in% c("nlp", "lp", "hm", "cf")
  x[!test] <- NA
  introduceNA(x, test)
  x
}

#' @rdname plausibility_methods
#' @export
checkVA.snellen <- function(x, ...){
  x <- convertQuali(x, ...)
  test <- is.na(x) | grepl("/", x)
  x_old <- x
  x[!test] <- NA
  introduceNA(x_old, !test)
  x
}

#' @rdname plausibility_methods
#' @export
checkVA.snellendec <- function(x, ...){
  x <- convertQuali(x, ...)
  x_num <- suppressWarnings(as.numeric(x))
  newna <- as.logical(is.na(x_num) - is.na(x))
  test <- x_num >= 0 & x_num <= 2
  newtest <- ifelse(is.na(test), newna, !test)
  x_num[!test] <- NA
  introduceNA(x, newtest)
  x_num
}

#' @rdname plausibility_methods
#' @export
checkVA.logmar <- function(x, ...){
  x <- convertQuali(x, ...)
  x_num <- suppressWarnings(as.numeric(x))
  newna <- as.logical(is.na(x_num) - is.na(x))
  test <- x_num >= -0.3 & x_num <= 3
  newtest <- ifelse(is.na(test), newna, !test)
  x_num[!test] <- NA
  introduceNA(x, newtest)
  x_num
}

#' @rdname plausibility_methods
#' @export
checkVA.etdrs <- function(x, ...){
  x <- convertQuali(x, ...)
  x_int <- suppressWarnings(as.integer(x))
  newna <- as.logical(is.na(x_int) - is.na(x))
  test <- x == x_int & x_int >= 0 & x_int <= 100
  newtest <- ifelse(is.na(test), newna, !test)
  x_int[!test] <- NA
  introduceNA(x, newtest)
  x_int
}

#' @rdname plausibility_methods
#' @export
checkVA.default <- function(x, ...){
  x
}

#' introduce NA for implausible VA entries
#' @name introduceNA
#' @param x vector
#' @param test plausibility test
#' @return vector
#' @keywords internal
introduceNA <- function(x, test){
  if(any(test)){
    message(paste0(" NA introduced (", sum(test, na.rm = TRUE),
                   "x) for values: ",
                   paste(unique(x[test]), collapse = ", ")))
  }
}

#' convert quali entries
#' @name convertQuali
#' @param x vector
#' @param va_class to which class
#' @return vector
#' @keywords internal
convertQuali <- function(x, va_class, type = NULL){
  if(!is.null(type)){
    va_class <- paste(va_class, type, sep = "_")
  }
  if (any(x %in% c("nlp", "lp", "hm", "cf"))) {
    x_quali <- va_chart[[va_class]][match(x, va_chart$quali[1:4])]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  x
}


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
#'   - Snellen decimal must be > 0 and <= 2
#'   - ETDRS must be >= 0 and <= 100
#'   - logMAR must be >= -0.3 and <= 3.0
#'   - Qualitative must be PL, LP, NLP, NPL, HM, CF (any case allowed)
#'
#' Any element which is implausible / not recognized will be converted to NA
#' @return vector with visual acuity of class `va`. See also "VA classes"
#' @family VA converter
#' @keywords internal
checkVA <- function (x, ...) {
  UseMethod("checkVA", x)
}

#' @rdname plausibility_methods
#' @keywords internal
checkVA.quali <- function(x, ...){
  test <- is.na(x) | x %in% c("nlp", "lp", "hm", "cf")
  x[!test] <- NA
  x
}

#' @rdname plausibility_methods
#' @keywords internal
checkVA.snellen <- function(x, ...){
  test <- is.na(x) | grepl("/", x)
  x_old <- x
  x[!test] <- NA
  x
}

#' @rdname plausibility_methods
#' @keywords internal
checkVA.snellendec <- function(x, ...){
  x_num <- suppressWarnings(as.numeric(x))
  test <- x_num > 0 & x_num <= 2
  x_num[!test] <- NA
  class(x_num) <- c(class(x_num), "snellendec")
  x_num
}

#' @rdname plausibility_methods
#' @keywords internal
checkVA.logmar <- function(x, ...){
  x_num <- suppressWarnings(as.numeric(x))
  test <- x_num >= -0.3 & x_num <= 3
  x_num[!test] <- NA
  class(x_num) <- c(class(x_num), "logmar")
  x_num
}

#' @rdname plausibility_methods
#' @keywords internal
checkVA.etdrs <- function(x, ...){
  x_int <- suppressWarnings(as.integer(x))
  test_int <- x == x_int & x_int >= 0 & x_int <= 100
  test_quali <-  x %in% c(-111L, -222L)
  test <- test_int | test_quali
  x_int[!test] <- NA
  class(x_int) <- c(class(x_int), "etdrs")
  x_int
}

#' @rdname plausibility_methods
#' @keywords internal
checkVA.default <- function(x, ...){
  x
}

#' VA conversion generics
#' @name va_generics
#' @description S3 generics for VA conversion
#' @param x vector of visual acuities
#' @family VA generics
#' @family VA methods
va_logmar <- function (x, ...) {
  UseMethod("tologMAR", x)
}

#' @rdname va_generics
va_etdrs <- function (x, ...) {
  UseMethod("toETDRS", x)
}

#' @rdname va_generics
#' @param snellen which Snellen notation to display. Either "sn", "m" or "dec"
va_snellen <- function (x, snellen = "ft") {
  UseMethod("toSnellen", x)
}

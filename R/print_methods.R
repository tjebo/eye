#' print.snellen
#' @name print_methods
#' @description S3 methods for VA classes "snellen", "logmar" and "etdrs".
#' **snellen** is always also a character class- because it is more categorical
#' than continuous. **logmar** and **etdrs** are both numerics
#' (logMAR is double, etdrs is integer).
#' @param x vector
#' @export
#' @family print methods
print.snellen <- function(x) {
  print.default(as.character(x))
}
#' print.logmar
#' @rdname print_methods
#' @param x vector
#' @export
#' @family print methods
print.logmar <- function(x) {
  print.default(as.numeric(x))
}
#' print.etdrs
#' @rdname print_methods
#' @param x vector
#' @export
#' @family print methods
print.etdrs <- function(x) {
  print.default(as.integer(x))
}


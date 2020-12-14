#' VA class conversion
#' @name VAclassConversion
#' @description Simple convenience wrapper around [va] to get desired VA class
#' @param x vector of visual acuities
#' @param ... parameters passed to [va]
#' @return vector with visual acuity of class to_(...)
#'   See also [convertVA]: "VA classes"
#' @family VA converter
#' @export
#'
to_logmar <- function(x, ...){
  va(x, to = "logmar", ...)
}

#' @rdname VAclassConversion
#' @export
to_etdrs <- function(x, ...){
  va(x, to = "etdrs", ...)
}

#' @rdname VAclassConversion
#' @export
to_snellen <- function(x, ...){
  va(x, to = "snellen", ...)
}


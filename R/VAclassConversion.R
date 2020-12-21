#' VA conversion wrapper
#' @name VAwrapper
#' @description Simple convenience wrapper around [va] to get desired VA class
#' @param x vector of visual acuities
#' @param ... parameters passed to [va]
#' @section VA conversion:
#' For details see [va] and [convertVA]
#' @return vector with visual acuity of class as_(...) or to_(...)
#'   See also [convertVA]: "VA classes"
#' @family VA converter
#' @examples
#' x <- c(23, 56, 74, 58) ## ETDRS letters
#' to_logmar(x)
#' to_snellen(x)
#' to_snellen(x, type = "dec")
#'
#' x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/50", "  ", ".", "-", "NULL")
#' to_snellen(x, from = "snellendec")
#' to_snellen(x, from = "etdrs")
#' to_snellen(x, from = "logmar")
#' @export
#'
to_logmar <- function(x, ...){
  va(x, to = "logmar", ...)
}

#' @rdname VAwrapper
#' @export
to_etdrs <- function(x, ...){
  va(x, to = "etdrs", ...)
}

#' @rdname VAwrapper
#' @export
to_snellen <- function(x, ...){
  va(x, to = "snellen", ...)
}

#' @rdname VAwrapper
#' @export
as_logmar <- to_logmar

#' @rdname VAwrapper
#' @export
as_etdrs <- to_etdrs

#' @rdname VAwrapper
#' @export
as_snellen <- to_snellen


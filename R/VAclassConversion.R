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
as_logmar <- function(x, ...){
  va(x, to = "logmar", ...)
}

#' @rdname VAwrapper
#' @export
as_etdrs <- function(x, ...){
  va(x, to = "etdrs", ...)
}

#' @rdname VAwrapper
#' @export
as_snellen <- function(x, ...){
  va(x, to = "snellen", ...)
}


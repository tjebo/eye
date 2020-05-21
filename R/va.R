#' va
#' @name va
#' @description Conversion of va notations
#' @family ophthalmic functions
#' @examples
#' eyes(amd)
#' @details formula from (gregori)
#' Although a mean value
#' expressed as logMAR units is sensibly back-converted to
#' the Snellen fraction, the SD cannot be easily backconverted
#' 1/200 considered CF. From 20/800 no linear relation to ETDRS.
#' snellen 2/200 suggest to be equivalent to 2 letters,
#' and 1/200 to 0 letters
#' logMAR <- round(-1 * log10(dec_sn), 2)
#' ETDRS <- round(85 + 50 * log10(dec_sn), 0)
#' @export

mean <- function (x, ...) {
  UseMethod("mean", x)
}

x <- structure(1, class = letters)
bar <- function(x) UseMethod("bar", x)
bar.z <- function(x) "z"
bar(x)



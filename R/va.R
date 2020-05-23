#' va
#' @name va
#' @description VA notation conversion
#' @param x vector with VA values
#' @param from String, from which notation to convert (See details)
#' @param to String, to which notation to convert. Default logMAR (See details)
#' @family ophthalmic functions
#' @details **from** if not provided (default), `va` will try to guess.
#'   This will work in most cases, but can fail due to weird, unpredicted ways
#'   to enter and save data. For more control, provide string:
#'   Either "ETDRS", "logMAR" or "snellen" (any case allowed).
#'   **to** Supported: "ETDRS", "logMAR" or "snellen" (any case allowed).
#' @examples
#' TBC
#' @details
#' Although a mean value
#' expressed as logMAR units is sensibly back-converted to
#' the Snellen fraction, the SD cannot be easily backconverted
#' 1/200 considered CF. From 20/800 downwards no linear relation to ETDRS.
#' snellen 2/200 suggest to be equivalent to 2 letters,
#' and 1/200 to 0 letters
#'
#'
#' @export
va <- function(x, from = NULL, to = "logmar"){
if(is.null(from)){
  class_va <- which_va(x)
} else if (length(to) != 1 |
           length(from) != 1 |
           !tolower(from) %in% c("ETDRS", "logMAR", "snellen")){
  stop("\"from\" or \"to\" should be only one of \"etdrs\", \"logmar\"
  or \"snellen\" - any case allowed.", call. = FALSE)
  class_va <- tolower(from)
}
  if(class_va == to){
    stop("VA already in the desired notation", call. = FALSE)
  }
  class(x) <- class_va

  eval(sym(paste0("va_", to)))(x)

}


#' VA conversion
#' @name va_methods
#' @description S3 methods for VA conversion
#' @param x vector of visual acuities
#' @family va conversion methods
va_logmar <- function (x) {
  UseMethod("tologMAR", x)
}

#' va_etdrs
#' @rdname va_methods
#' @param x vector of visual acuities
#' @family va conversion methods
va_etdrs <- function (x) {
  UseMethod("toETDRS", x)
}

#' va_snellen
#' @rdname va_methods
#' @param x vector of visual acuities
#' @param snellen which Snellen notation to display. Either "sn", "m" or "dec"
#' @family va conversion methods
va_snellen <- function (x, snellen = "ft") {
  UseMethod("toSnellen", x)
}

#' tologMAR.snellen
#' @rdname va_methods
#' @param x vector of class snellen
#' @family va conversion methods
tologMAR.snellen <- function(x){
  x <- as.character(x)
  snellen_frac <- with(read.table(text = x, sep = "/"), V1 / V2)
  logMAR <- round(-1 * log10(snellen_frac), 2)
  logMAR
}

#' toETDRS.snellen
#' @rdname va_methods
#' @param x vector of class snellen
#' @details Only approximate eqivalent.
#'   Using formula suggested by Gregori et al.
#' @family va conversion methods
toETDRS.snellen <- function(x){
  x <- as.character(x)
  snellen_frac <- with(read.table(text = x, sep = "/"), V1 / V2)
  ETDRS <- round(85 + 50 * log10(snellen_frac), 0)
  ETDRS[snellen_frac <= 0.02 & snellen_frac > 0.005] <- 2
  ETDRS[snellen_frac <= 0.005] <- 0
  ETDRS
}

#' toSnellen.logmar
#' @rdname va_methods
#' @param x vector of class logMAR
#' @param snellen which Snellen notation to display. Either "sn", "m" or "dec"
#' @details Only approximate eqivalent.
#'   Rounding logMAR to nearest first digit and converting
#'   to snellen using the va conversion chart
#' @family va conversion methods
toSnellen.logmar <- function(x, snellen = "ft"){
x <- round(as.numeric(x), 1)
col = paste("snellen", snellen, sep = "_")
snellen <- va_chart[[col]][match(x, as.numeric(va_chart$logMAR))]
snellen
}

#' toETDRS.logmar
#' @rdname va_methods
#' @param x vector of class logMAR
#' @family va conversion methods
#' @details Only approximate eqivalent.
#'   Rounding logMAR to nearest first digit and
#'   converting to ETDRS letters using the va conversion chart
toETDRS.logMAR <- function(x){
  x <- round(as.numeric(x), 1)
  ETDRS <- eye::va_chart[["ETDRS"]][match(x, as.numeric(eye::va_chart[["logMAR"]]))]
  ETDRS
}

#' tologMAR.etdrs
#' @rdname va_methods
#' @param x vector of class ETDRS
#' @family va conversion methods
#' @details Approximate eqivalent based on formula in Beck et al.
tologMAR.etdrs <- function(x){
  logMAR <- (-0.02 * x) + 1.7
  logMAR
}

#' toSnellen.etdrs
#' @rdname va_methods
#' @param x vector of class ETDRS
#' @param snellen which Snellen notation to display. Either "sn", "m" or "dec"
#' @family va conversion methods
#' @details Approximate eqivalent based on formula in Beck et al.
#'   First coverting to logMAR, rounding this to the closest first digit,
#'   converting this to snellen using the chart.
toSnellen.etdrs <- function(x, snellen = "ft"){
   x <- round((-0.02 * x) + 1.7, 1)
    col = paste("snellen", snellen, sep = "_")
    snellen <- va_chart[[col]][match(x, as.numeric(va_chart$logMAR))]
    snellen
}

#' which_va
#' @rdname internals
#' @param x vector
#' @description internal check which type of va in order to assign class.
#' @family va conversion functions
#'
which_va <- function(x) {
  x <- suppressWarnings(as.character(x))
  if (all(x %in% c("NLP", "LP", "HM", "CF"))) {
    return("quali")
  }

  x_num <- suppressWarnings(as.numeric(x))
  if (all(grepl("/", x[!is.na(x)]))) {
    return("snellen")
  }

  if (all(is.na(x_num))) {
    stop("Which notation? No typical notation found.", call. = FALSE)
  }
  x_numval <- x_num[!is.na(x_num)]
  if (length(x_numval) < length(x[!is.na(x)])) {
    warning("Numeric notation containing character values (removed!)-
            Were those Snellen? CF/HM/LP/NLP?", call. = FALSE)
  }

  if (all(x_numval == as.integer(x_numval))) {
    if (all(x_numval >= 0) & all(x_numval <= 100)) {
      warning("Guessing ETDRS", call. = FALSE)
      return("etdrs")
    } else {
      stop("This seems to be ETDRS notation but there are unplausible values.
         Check your data.", call. = FALSE)
    }
  }
  if (all(x_num %in% suppressWarnings(as.numeric(va_chart$snellen_dec)))) {
    warning("Guessing snellen decimal", call. = FALSE)
    return("snellen")
  } else if (any(x_numval < -0.3 | any(x_numval > 4.2))) {
    stop("VA notation not clear (Unplausible values). Check your data.", call. = FALSE)
  } else {
    return("logmar")
  }
}


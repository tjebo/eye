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
#' @importFrom rlang sym
#'
#' @export
va <- function(x, from = NULL, to = "logmar") {
  to <- tolower(to)

  if (length(to) != 1 | !to %in% c("etdrs", "logmar", "snellen")) {
    stop("\"to\" should be only one of \"etdrs\", \"logmar\"
  or \"snellen\" - any case allowed.", call. = FALSE)
  }
  if(is.character(x)|is.factor(x)){
  x <- convert_na_va(x)
  }
  guess_va <- which_va(x)
  if (guess_va == "failed" & is.null(from)) {
    stop("Failed to autodetect VA class. Fix with from argument")
  }

  if (!is.null(from)) {
    class_va <- tolower(from)
    if (length(class_va) != 1 | !class_va %in% c("etdrs", "logmar", "snellen")) {
      stop("\"from\" should be only one of \"etdrs\", \"logmar\"
  or \"snellen\" - any case allowed.", call. = FALSE)
    }
    if (guess_va == "failed") {
      guess_va <- class_va
      warning("Failed to autodetect VA class - relying on from argument.
              Check your data for weird values.", call. = FALSE)
    }
    if (class_va != guess_va) {
      stop(paste(class_va, "class not plausible. Check data"),
        call. = FALSE
      )
    }
  } else {
    class_va <- guess_va
  }
  if (class_va == to) {
    stop("VA already in the desired class", call. = FALSE)
  }

  class(x) <- class_va

  eval(rlang::sym(paste0("va_", to)))(x)
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
  snellen_frac <-
    sapply(strsplit(x, "/"),
           function(x) { x <- suppressWarnings(as.numeric(x)); x[1] / x[2]})
  logMAR <- round(-1 * log10(snellen_frac), 2)
  if(any(x %in% set_quali())){
    x_quali <- va_quali$logMAR[match(x, va_quali$quali)]
    logMAR <- ifelse(is.na(logMAR), x_quali, logMAR)
  }
  class(logMAR) <- c("logmar","numeric")
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
  snellen_frac <-
    sapply(strsplit(x, "/"),
           function(x) { x <- suppressWarnings(as.numeric(x)); x[1] / x[2]})
  ETDRS <- round(85 + 50 * log10(snellen_frac), 0)
  ETDRS[snellen_frac <= 0.02 & snellen_frac > 0.005] <- 2
  ETDRS[snellen_frac <= 0.005] <- 0
  if(any(x %in% set_quali())){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    ETDRS <- ifelse(is.na(ETDRS), x_quali, ETDRS)
  }
  class(ETDRS) <- c("etdrs", "integer")
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
x_num <- suppressWarnings(round(as.numeric(x), 1))
if(any(x %in% set_quali())){
  x_quali <- as.numeric(va_quali$logMAR[match(x, va_quali$quali)])
  x_num <- ifelse(is.na(x_num), x_quali, x_num)
}
col = paste("snellen", snellen, sep = "_")
snellen <- va_chart[[col]][match(x_num, as.numeric(va_chart$logMAR))]
class(snellen) <- c("snellen", "character")
snellen
}

#' toETDRS.logmar
#' @rdname va_methods
#' @param x vector of class logMAR
#' @family va conversion methods
#' @details Only approximate eqivalent.
#'   Rounding logMAR to nearest first digit and
#'   converting to ETDRS letters using the va conversion chart
toETDRS.logmar <- function(x){
  x_num <- suppressWarnings(round(as.numeric(x), 1))
  if(any(x %in% set_quali())){
    x_quali <- as.numeric(va_quali$logMAR[match(x, va_quali$quali)])
    x_num <- ifelse(is.na(x_num), x_quali, x_num)
  }
  ETDRS <- eye::va_chart[["ETDRS"]][match(x_num, as.numeric(eye::va_chart[["logMAR"]]))]
  class(ETDRS) <- c("etdrs", "integer")
  ETDRS
}

#' tologMAR.etdrs
#' @rdname va_methods
#' @param x vector of class ETDRS
#' @family va conversion methods
#' @details Approximate eqivalent based on formula in Beck et al.
tologMAR.etdrs <- function(x){
  if(any(x %in% set_quali())){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x <- ifelse(is.na(x), x_quali, x)
  }
  logMAR <- (-0.02 * x) + 1.7
  class(logMAR) <- c("logmar","numeric")
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
  if(any(x %in% set_quali())){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x <- ifelse(is.na(x), x_quali, x)
  }
   x <- round((-0.02 * x) + 1.7, 1)
    col = paste("snellen", snellen, sep = "_")
    snellen <- va_chart[[col]][match(x, as.numeric(va_chart$logMAR))]
    class(snellen) <- c("snellen", "character")
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

  if (all(x[!is.na(x)] %in% set_quali())) {
    return("quali")
  }
  x_noquali <- x[!x %in% set_quali()]
  x_num <- suppressWarnings(as.numeric(x_noquali))
  if (all(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("snellen")
  }

  if (all(is.na(x_num))) {
    warning("Guess snellen? Needs to be in format \"x/y\" for conversion",
            call. = FALSE)
    return("failed")
  }
  x_numval <- x_num[!is.na(x_num)]
  if (length(x_numval) < length(x_noquali[!is.na(x_noquali)])) {
    warning("Removed characters from numeric VA class -
            Mixed with snellen or CF/HM/LP/NLP?", call. = FALSE)
  }

  if (all(x_numval == as.integer(x_numval))) {
    if (all(x_numval >= 0) & all(x_numval <= 100)) {
      message("Guessing ETDRS")
      return("etdrs")
    } else {
      warning("Guess ETDRS? Values out of range. Check your data.",
              call. = FALSE)
      return("failed")
    }
  }
  if (all(x_num %in% suppressWarnings(as.numeric(va_chart$snellen_dec)))) {
    message("Guessing snellen decimal")
    return("snellen")
  } else if (any(x_numval < -0.3 | any(x_numval > 4.2))) {
    warning("Guess logMAR? Values out of range. Check your data.",
            call. = FALSE)
    return("failed")
  } else {
    return("logmar")
  }
}

#' convert_na_va
#' @rdname internals
#' @param x vector
#' @family va conversion functions
#'
convert_na_va <- function(x){
  x[isNAstring(x)] <- NA_character_
  x
}

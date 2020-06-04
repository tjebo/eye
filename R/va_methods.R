#' VA conversion methods
#' @name va_methods
#' @description S3 methods for VA conversion
#' @param x vector of visual acuities
#' @param to to which VA class to convert
#' @param ... further arguments passed to methods
#' @details
#' VA can be snellen feet/meter/decimal, logMAR, ETDRS, or
#' "qualitative" (Couting fingers, etc.)
#'
#'   - Snellen fractions need to be either form 6/x or 20/x
#'   - ETDRS must be between 0 and 100
#'   - logMAR must be between -0.3 and 3.0
#'   - Qualitative must be PL, LP, NLP, NPL, HM, CF (any case allowed)
#'
#' Any element which is not recognized will be converted to NA
#'
#' For other conversion rules see [va]
#' @section Conversion:
#' Conversion to snellen notation is not really an exact science,
#' considering the somewhat peculiar steps in the historic (but widely used)
#' snellen charts, and snellen are not useful for statistics, therefore
#' I consider it a conversion out of "interest" and for a ballpoint
#' where the VA would be, rather than an exact science.
#'
#' Therefore, I chose to convert to the closest "equivalent"
#' snellen notation for a full line for both ETDRS and logMAR.
#' Conversion is done based on the conversion chart [va_chart].
#'
#' Further:
#' - logMAR to ETDRS: logMAR rounded to the first digit and converted with
#' the chart.
#' - Snellen to logMAR: logMAR = -1 * log10(snellen_frac)
#' - Snellen to ETDRS: ETDRS = 85 + 50 * log10(snellen_frac)
#' [Gregori et al.](https://doi.org/10.1097/iae.0b013e3181d87e04).
#' - ETDRS to logMAR: logMAR = -0.02 * etdrs + 1.7
#' [Beck et al.](https://doi.org/10.1016/s0002-9394(02)01825-1)
#' @family VA converter
#' @export
convertVA <- function (x, to, ...) {
  UseMethod("convertVA", x)
}

#' @rdname va_methods
#' @param snellnot which snellen notation. One of "ft", "m" or "dec"
#' @export
#'
convertVA.quali <- function(x, to, snellnot, ...){
  x <- tolower(x)
  if(to == "snellen"){
  col <- paste(to, snellnot, sep = "_")
  new_va <- va_quali[[col]][match(x, va_quali$quali)]
  } else {
  new_va <- va_quali[[to]][match(x, va_quali$quali)]
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @export
#'
convertVA.snellen <- function(x, to, snellnot, ...){
  x <- tolower(x)
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$snellen_ft[match(x, va_quali$quali)]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  parse_snellen <- strsplit(x, "/")
  snellen_frac <- sapply(parse_snellen,
                         function(x) {x <- suppressWarnings(as.numeric(x)); x[1] / x[2]})
  if(to == "logmar"){
    new_va <- as.numeric(round(-1 * log10(snellen_frac), 2))
  } else if (to == "etdrs"){
    new_va <- round(85 + 50 * log10(snellen_frac), 0)
    new_va[snellen_frac <= 0.02 & snellen_frac > 0.005] <- 2
    new_va[snellen_frac <= 0.005] <- 0
    new_va <- as.integer(new_va)
  } else if(to == "snellen"){
    looksnellen <- c("6" = "m", "20" = "ft")
    snellen_type <- sapply(parse_snellen, function(x) looksnellen[x[1]])
    from <- paste(to, snellen_type, sep = "_")
    col <- paste(to, snellnot, sep = "_")
    new_va <- mapply(FUN = function(x, fromstr){
      va_chart[[col]][match(x, va_chart[[fromstr]])]
    }, x, from, USE.NAMES = FALSE)
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @export
#'
convertVA.logmar <- function(x, to, snellnot, ...){
  x <- tolower(x)
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$snellen_ft[match(x, va_quali$quali)]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  x_num <- suppressWarnings(as.numeric(x))
  x_num[x_num < -0.3 | x_num > 3] <- NA
  x_num <- round(x_num, 1)

  if(to == "snellen"){
    col <- paste(to, snellnot, sep = "_")
    new_va <- va_chart[[col]][match(x_num, as.numeric(va_chart$logmar))]
  } else {
  new_va <- va_chart[[to]][match(x_num, as.numeric(va_chart$logmar))]
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}


#' @rdname va_methods
#' @export
convertVA.etdrs <- function(x, to, snellnot, ...){
  x <- tolower(x)
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$etdrs[match(x, va_quali$quali)]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  x_int <- suppressWarnings(as.integer(x))
  x_int[x_int < 0 | x_int > 100] <- NA

  if(to == "snellen"){
    col <- paste(to, snellnot, sep = "_")
    b <- va_chart$etdrs[!is.na(va_chart$etdrs)]
    round_etdrs <- as.integer(b[findInterval(x_int, (b[-length(b)] + b[-1]) / 2) + 1])
    new_va <- va_chart[[col]][match(round_etdrs, as.integer(va_chart$etdrs))]
  } else if(to == "logmar"){
    new_va <- (-0.02 * x_int) + 1.7
    new_va <- round(as.numeric(new_va), 2)
  } else {
    new_va <- va_chart[[to]][match(x_int, as.numeric(va_chart$etdrs))]
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @export
convertVA.default <- function(x, to, snellnot, ...){
NA
}


#' VA conversion methods
#' @name va_methods
#' @description S3 methods for VA conversion
#' @param x vector of visual acuities
#' @param to to which VA class to convert
#' @param ... further arguments passed to methods
#' @details
#' VA can be snellen feet/meter/decimal, logMAR, ETDRS, or
#' "qualitative" (Counting fingers, etc.)
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
#' Although there seems to be no good statistical reason to convert
#' back to Snellen, it is a very natural thing to eye specialists to think
#' in Snellen. A conversion to snellen gives a good gauge of how the visual
#' acuity for the patients are. However, back-conversion should not be
#' considered an exact science and any attempt to use formulas will result
#' in very weird Snellen values that have no correspondence to common charts.
#' Therefore, Snellen matching the nearest ETDRS and logMAR value in
#' the [va_chart] are used.
#'
#' Further:
#' - logMAR to ETDRS: logMAR rounded to the first digit and converted with
#' the chart.
#' - Snellen to logMAR: logMAR = -1 * log10(snellen_frac)
#' - Snellen to ETDRS: ETDRS = 85 + 50 * log10(snellen_frac)
#' [Gregori et al.](https://doi.org/10.1097/iae.0b013e3181d87e04).
#' - ETDRS to logMAR: logMAR = -0.02 * etdrs + 1.7
#' [Beck et al.](https://doi.org/10.1016/s0002-9394(02)01825-1)
#' @section VA classes:
#' convert_VA returns a vector of three classes:
#'   1. `va`
#'   1. One of `snellen`, `logmar`, `etdrs` or `quali`.
#'   1. Either of `character` (for Snellen and qualitative),
#'       `numeric` (for logMAR), or `integer` (for ETDRS).
#' @return vector with visual acuity of class `va`. See also "VA classes"
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
#' @param logmarstep how plus/minus entries are evaluated. Default to
#'   increase/decrease snellen fractions by lines. If TRUE, each snellen
#'   optotype will be considered equivalent to 0.02 logmar or 1 ETDRS
#'   letter (assuming 5 letters in a row in a chart)
#' @export
#'
convertVA.snellen <- function(x, to, snellnot, logmarstep, ...) {
  if (to == "snellen" & logmarstep) {
    warning("logmarstep should be FALSE when to = \"snellen\"", call. = FALSE)
  }
  if (any(x %in% c("nlp", "lp", "hm", "cf"))) {
    x_quali <- va_quali$snellen_ft[match(x, va_quali$quali)]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  x_num <- suppressWarnings(as.numeric(x))

  if (any(!is.na(x_num))) {
    x_dec <- va_chart$snellen_ft[match(x_num, as.numeric(va_chart$snellen_dec))]
    x <- ifelse(!is.na(x_dec), x_dec, x)
  }
  x_split <- strsplit(x, "(?<=.)(?=[-\\+])", perl = TRUE)
  if (!logmarstep) {
    x <- snellensteps(x_split)
    parse_snellen <- strsplit(x, "/")
  } else {
    x <- sapply(x_split, function(x) x[[1]])
    plussplit <- sapply(
      x_split,
      function(x) suppressWarnings(as.integer(x[2]))
    )
    plussplit[is.na(plussplit)] <- 0
    parse_snellen <- strsplit(x, "/")
  }
  snellen_frac <- sapply(
    parse_snellen,
    function(x) {
      x <- suppressWarnings(as.numeric(x))
      x[1] / x[2]
    }
  )

  if (to == "logmar") {
    new_va <- as.numeric(round(-1 * log10(snellen_frac), 2))
    if (logmarstep) {
      new_va <- new_va + 0.02 * plussplit
    }
  } else if (to == "etdrs") {
    new_va <- round(85 + 50 * log10(snellen_frac), 0)
    new_va[snellen_frac <= 0.02 & snellen_frac > 0.005] <- 2
    new_va[snellen_frac <= 0.005] <- 0
    new_va <- as.integer(new_va)
    if (logmarstep) {
      new_va <- new_va + plussplit
    }
  } else if (to == "snellen") {
    looksnellen <- c("6" = "m", "20" = "ft")
    snellen_type <- sapply(parse_snellen, function(x) looksnellen[x[1]])
    from <- paste(to, snellen_type, sep = "_")
    col <- paste(to, snellnot, sep = "_")
    new_va <- mapply(FUN = function(x, fromstr) {
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
  if(any(x %in% c("nlp", "lp", "hm", "cf"))){
    x_quali <- va_quali$logmar[match(x, va_quali$quali)]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  x_num <- suppressWarnings(as.numeric(x))
  x_num[x_num < -0.3 | x_num > 3] <- NA

  if(to == "snellen"){
    col <- paste(to, snellnot, sep = "_")
    new_va <- va_chart[[col]][match(round(x_num, 1), as.numeric(va_chart$logmar))]
  } else if(to == "etdrs"){
  new_va <- va_chart[[to]][match(round(x_num, 1), as.numeric(va_chart$logmar))]
  } else{
    new_va <- x_num
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @export
convertVA.etdrs <- function(x, to, snellnot, ...){
  x <- tolower(x)
  if(any(x %in% c("nlp", "lp", "hm", "cf"))){
    x_quali <- va_quali$etdrs[match(x, va_quali$quali)]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  x_int <- suppressWarnings(as.integer(x))
  true_int <- x == x_int
  x_int[x_int < 0 | x_int > 100] <- NA
  x_int[!true_int] <- NA

  if(to == "snellen"){
    col <- paste(to, snellnot, sep = "_")
    b <- va_chart$etdrs[!is.na(va_chart$etdrs)]
    round_etdrs <- as.integer(b[findInterval(x_int, (b[-length(b)] + b[-1]) / 2) + 1])
    new_va <- va_chart[[col]][match(round_etdrs, as.integer(va_chart$etdrs))]
  } else if(to == "logmar"){
    new_va <- (-0.02 * x_int) + 1.7
    new_va <- round(as.numeric(new_va), 2)
  } else if(to == "etdrs"){
    new_va <- x_int
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @export
convertVA.default <- function(x, to, snellnot, ...){
NA
}


#' Convert plus minus entries
#' @name snellen_steps
#' @param y Vector with VA entries of class snellen - needs to be in
#' format xx/yy
#' @description used in conversion method for class snellen
#' - Removing "plus" and "minus" from snellen notation
#'     - if entry -1 to +3 : take same Snellen value
#'     - if <= -2 : take Snellen value one line below
#'     - if >+3 (unlikely, but unfortunately not impossible):
#'     Snellen value one line above
#' @section snellen_steps:
#' Snellen are unfortunately often entered with "+/-", which is a
#' violation of a psychophysical method designed to assign one
#' unambiguous value to visual acuity, with
#' non-arbitrary thresholds based on psychometric functions. Therefore,
#' transforming "+/-" notation to actual results is in itself
#' problematic and the below suggestion to convert it will remain an
#' approximation to the most likely "true" result. Even more so, as the
#' given conditions should work for charts with
#' 4 or 5 optotypes in a line, and visual acuity is not always tested
#' on such charts. Yet, I believe that the approach is still better than
#' just omitting the letters or (worse) assigning a missing value to those
#' entries.
#' @family VA converter
#' @return character vector of Snellen entries
#' @seealso
#' https://en.wikipedia.org/wiki/Psychometric_function

snellensteps <- function(y){
  parsed_elem <- sapply(y, function(x) {
    snellen_split <- x[1]
    plusminus <- suppressWarnings(as.integer(x[2]))
    snellen_type <- if (snellen_split %in% va_chart$snellen_ft) {
      "snellen_ft"
    } else if (snellen_split %in% va_chart$snellen_m) {
      "snellen_m"
    } else {
      NA
    }
    if (plusminus <= 3 & plusminus >= -1 |
        is.na(plusminus) | is.na(snellen_type)) {
      new_elem <- snellen_split
    } else if (plusminus <- 1) {
      ind <- match(snellen_split, va_chart[[snellen_type]]) - 1
      new_elem <- va_chart[[snellen_type]][ind]
    } else if (plusminus > 3) {
      ind <- match(snellen_split, va_chart[[snellen_type]]) + 1
      new_elem <- va_chart[[snellen_type]][ind]
    }
    new_elem
  })
  parsed_elem
}


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
#' @section Conversion:
#' For other conversion and theory behind conversion rules see [va]
#' section VA conversion.
#'
#' @section plus/minus entries:
#' The following rules for plus minus notations will be applied:
#'
#' - if entry -2 to +2 : take same Snellen value
#' - if < -2 : take Snellen value one line below
#' - if > +2: Snellen value one line above
#'
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
#'
#' @return vector with visual acuity of class `va`. See also "VA classes"
#' @family VA converter
#' @keywords internal
convertVA <- function (x, ...) {
  UseMethod("convertVA", x)
}

#' @rdname va_methods
#' @param type which snellen notation. One of "ft", "m" or "dec"
#' @keywords internal
#'
convertVA.quali <- function(x, to, type, ...){
    matchcol <- paste0(to, type)
    new_va <- va_chart[[matchcol]][match(x, va_chart$quali[1:4])]
    if(to == "etdrs"){
      new_va[new_va < 0] <- 0L
    }
    class(new_va) <- c(to, "va", class(new_va))
    new_va

}

#' @rdname va_methods
#' @param smallstep how plus/minus entries are evaluated. Default to
#'   increase/decrease snellen fractions by lines. If TRUE, each snellen
#'   optotype will be considered equivalent to 0.02 logmar or 1 ETDRS
#'   letter (assuming 5 letters in a row in a chart)
#' @keywords internal
convertVA.snellendec <- function(x, to, type, ...) {
  if (to == "logmar") {
    new_va <- round(-1 * log10(x), 2)
  } else if (to == "snellen") {
    matchcol <- paste0(to, type)
    # rounding to nearest snellen decimal
    b <- 1000 * sort(as.numeric(va_chart$snellendec))
    round_dec <-
      as.integer(b[findInterval(1000 * x, (b[-length(b)] + b[-1]) / 2) + 1])

    new_va <- va_chart[[matchcol]][match(
      round_dec / 1000,
      as.numeric(va_chart$snellendec)
    )]
  } else if (to == "etdrs") {
    new_va <- round(85 + 50 * log10(x), 0)
    new_va[x <= 0.02 & x > 0.005] <- 2
    new_va[x <= 0.005] <- 0
    new_va <- as.integer(new_va)
  } else {
    new_va <- x
  }
  class(new_va) <- c(class(new_va), to)
  new_va
}

#' @rdname va_methods
#' @param smallstep how plus/minus entries are evaluated. Default to
#'   increase/decrease snellen fractions by lines. If TRUE, each snellen
#'   optotype will be considered equivalent to 0.02 logmar or 1 ETDRS
#'   letter (assuming 5 letters in a row in a chart)
#' @param noplus ignoring plus/minus entries and just returning the
#'     snellen fraction. This overrides the smallstep argument.
#' @keywords internal
#'
convertVA.snellen <- function(x, to, type, smallstep, noplus, ...) {
  if(noplus) {
    x_split <- strsplit(x, "(?<=.)(?=[-\\+])", perl = TRUE)
    snellenfrac <- sapply(x_split, `[[`, 1)
    logmarval <- round(-log10(parse_snellen(snellenfrac)), 1)
  } else {
  logmarval <- snellensteps(x, smallstep)
  }
  if (to == "logmar") {
      new_va <- logmarval
  } else if (to == "etdrs") {
    new_va <- round(85 - 50 * logmarval, 0)
    new_va[logmarval >= 1.7 & logmarval < 2.3] <- 2
    new_va[logmarval >= 2.3] <- 0
    new_va <- as.integer(new_va)
  } else if (grepl("snellen", to)) {
    matchcol <- paste0(to, type)
    # rounding to nearest logMAR
    x_num <- round(logmarval, 1)
    b <- 100*sort(va_chart$logmar)
    round_logmar <-
      as.integer(b[findInterval(100*x_num, (b[-length(b)] + b[-1]) / 2) + 1])
    new_va <- va_chart[[matchcol]][match(round(round_logmar/100, 1),
                                    as.numeric(va_chart$logmar))]
  } else {
    new_va <- x
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @keywords internal
#'
convertVA.logmar <- function(x, to, type, ...){
  if(to != "logmar"){
  matchcol <- paste0(to, type)
  x_num <- round(x, 1)
  b <- 100*sort(va_chart$logmar)
  round_logmar <-
    as.integer(b[findInterval(100*x_num, (b[-length(b)] + b[-1]) / 2) + 1])
  new_va <- va_chart[[matchcol]][match(round(round_logmar/100, 1),
                                       as.numeric(va_chart$logmar))]
  if(to == "etdrs"){
    new_va[new_va < 0] <- 0L
  }
  } else {
    new_va <- as.numeric(x)
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @keywords internal
convertVA.etdrs <- function(x, to, type, ...){
    if(to == "logmar"){
    x_num <- suppressWarnings(as.numeric(x))
    new_va <- (-0.02 * x_num) + 1.7
    ind_int <- which(x %in% c(0, -111L, -222L))
    new_va[ind_int] <- va_chart$logmar[match(x[ind_int], va_chart$etdrs)]
    new_va <- round(as.numeric(new_va), 2)
  } else if (to == "etdrs"){
    x[x < 0] <- 0
    new_va <- as.integer(x)
  } else if (to == "snellen"){
    match_col <- paste0(to, type)
    b <- va_chart$etdrs[!is.na(va_chart$etdrs)]
    round_etdrs <- as.integer(b[findInterval(x, (b[-length(b)] + b[-1]) / 2) + 1])
    new_va <- va_chart[[match_col]][match(round_etdrs, va_chart$etdrs)]
  } else {
    new_va <- as.integer(x)
  }
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

#' @rdname va_methods
#' @keywords internal
convertVA.default <- function(x, to, ...){
NA
}

#' Convert plus minus entries
#' @name snellen_steps
#' @param x Vector with VA entries of class snellen - needs to be in
#' format xx/yy
#' @param smallstep if plusminus shall be considered as logmar equivalent
#' @family VA converter
#' @return character vector of Snellen entries
#' @keywords internals
#' @seealso
#' https://en.wikipedia.org/wiki/Psychometric_function

snellensteps <- function(x, smallstep) {
  # x <- gsub(" ", "", x)
  x_split <- strsplit(x, "(?<=.)(?=[-\\+])", perl = TRUE)
  snellenfrac <- sapply(x_split, `[[`, 1)
  plusminus <- sapply(x_split, function(x) as.integer(x[2]))
  plusminus[is.na(plusminus)] <- 0
  logmar <- -log10(parse_snellen(snellenfrac))

  if (!smallstep) {
    newlogmar <- round(logmar, 1)
    cond1 <- plusminus < (-2) | is.na(newlogmar)
    cond2 <- plusminus > 2 | is.na(newlogmar)
    newlogmar[cond1] <- newlogmar[cond1] + 0.1
    newlogmar[cond2] <- newlogmar[cond2] - 0.1
  } else {
    newlogmar <- round(logmar, 2) - (0.02 * plusminus)
  }
  return(newlogmar)
}

#' parsing snellen fractions to numeric values
#' @param y vector
#' @keywords internals
parse_snellen <- function(y) {
  snellenfrac_ls <- strsplit(y, "/")
  sapply(snellenfrac_ls, function(x) {
    x <- suppressWarnings(as.numeric(x))
    x[1] / x[2]
  }
  )
}


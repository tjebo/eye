#' VA conversion methods
#' @name va_methods
#' @param x Vector with VA entries
#' @family VA methods
#' @export
tologMAR.quali <- function(x){
  logMAR <- va_quali$logMAR[match(x, va_quali$quali)]
  logMAR <- as.numeric(logMAR)
  class(logMAR) <- c("logmar","va",class(logMAR))
  logMAR
}

#' @rdname va_methods
#' @export
tologMAR.NA <- function(x){
  logMAR <- NA_integer_
  logMAR <- as.numeric(logMAR)
  class(logMAR) <- c("logmar","va",class(logMAR))
  logMAR
}

#' @rdname va_methods
#' @export
toSnellen.quali <- function(x, snellen = "ft"){
  col = paste("snellen", snellen, sep = "_")
  Snellen <- va_chart[[col]][match(x_num, as.numeric(va_chart$quali))]
  class(Snellen) <- c("snellen","va",class(Snellen))
  logMAR
}

#' @rdname va_methods
#' @export
toETDRS.quali <- function(x){
  ETDRS <- va_quali$ETDRS[match(x, va_quali$quali)]
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs","va",class(ETDRS))
  ETDRS
}

#' @rdname va_methods
#' @export
tologMAR.snellen <- function(x){
  x <- as.character(x)
  snellen_frac <-
    sapply(strsplit(x, "/"),
           function(x) { x <- suppressWarnings(as.numeric(x)); x[1] / x[2]})
  logMAR <- round(-1 * log10(snellen_frac), 2)
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$logMAR[match(x, va_quali$quali)]
    logMAR <- ifelse(is.na(logMAR), x_quali, logMAR)
  }
  logMAR <- as.numeric(logMAR)
  class(logMAR) <- c("logmar","va",class(logMAR))
  logMAR
}

#' @rdname va_methods
#' @description `toETDRS.snellen`: Using formula suggested by Gregori et al.
#'   ETDRS = 85 + 50 * log10(snellen_frac)
#' @export
toETDRS.snellen <- function(x){
  x <- as.character(x)
  snellen_frac <-
    sapply(strsplit(x, "/"),
           function(x) { x <- suppressWarnings(as.numeric(x)); x[1] / x[2]})
  ETDRS <- round(85 + 50 * log10(snellen_frac), 0)
  ETDRS[snellen_frac <= 0.02 & snellen_frac > 0.005] <- 2
  ETDRS[snellen_frac <= 0.005] <- 0
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    ETDRS <- ifelse(is.na(ETDRS), x_quali, ETDRS)
  }
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs","va", class(ETDRS))
  ETDRS
}

#' @rdname va_methods
#' @export

toSnellen.snellen <- function(x, snellen = "ft"){
  col = paste("snellen", snellen, sep = "_")
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali[[col]][match(x, va_quali$quali)]
    x <- ifelse(is.na(x), x_quali, x)
  }
  Snellen <- x
  class(Snellen) <- c("etdrs", "va", class(Snellen))
  Snellen
}

#' @rdname va_methods
#' @param snellen which Snellen notation to display. Either "sn", "m" or "dec"
#' @description `toSnellen.logmar`: Rounding logMAR to first digit and
#'   matching with Snellen entries in [va_chart]
#' @export
toSnellen.logmar <- function(x, snellen = "ft"){
  x_num <- suppressWarnings(as.numeric(x))
  x_num[x_num < -0.3 | x_num > 3] <- NA
  x_num <- round(x_num, 1)
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- as.numeric(va_quali$logMAR[match(x, va_quali$quali)])
    x_num <- ifelse(is.na(x_num), x_quali, x_num)
  }
  col = paste("snellen", snellen, sep = "_")
  Snellen <- va_chart[[col]][match(x_num, as.numeric(va_chart$logMAR))]
  class(Snellen) <- c("snellen", "va", class(Snellen) )
  Snellen
}

#' @rdname va_methods
#' @description `toETDRS.logmar`: Rounding logMAR to first digit and
#'   matching with ETDRS letters in [va_chart]
#' @export
toETDRS.logmar <- function(x){
  x_num <- suppressWarnings(as.numeric(x))
  x_num[x_num < -0.3 | x_num > 3] <- NA
  x_num <- round(x_num, 1)
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$logMAR[match(x, va_quali$quali)]
    x_num <- ifelse(is.na(x_num), x_quali, x_num)
  }
  ETDRS <- eye::va_chart[["ETDRS"]][match(x_num , as.numeric(eye::va_chart[["logMAR"]]))]
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs", "va", class(ETDRS))
  ETDRS
}

#' @rdname va_methods
#' @export
tologMAR.logmar <- function(x){
  x_num <- suppressWarnings(round(as.numeric(x), 2))
  x_num[x_num < -0.3 | x_num > 3] <- NA
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- as.numeric(va_quali$logMAR)[match(x, va_quali$quali)]
    x_num <- ifelse(is.na(x_num), x_quali, x_num)
  }
  logMAR <- as.numeric(x_num)
  class(logMAR) <- c("logmar", "va", class(logMAR))
  logMAR
}


#' @rdname va_methods
#' @export

toETDRS.etdrs <- function(x){
  x_int <- suppressWarnings(as.integer(x))
  x_int[x_int < 0 | x_int > 100] <- NA
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x_int <- ifelse(is.na(x_int), x_quali, x_int)
  }
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs", "va", class(ETDRS))
  ETDRS
}

#' @rdname va_methods
#' @description `tologMAR.etdrs`: Using formula in Beck et al.
#'   logMAR = -0.02 * ETDRS + 1.7
#' @export
tologMAR.etdrs <- function(x){
  x_int <- suppressWarnings(as.integer(x))
  x_int[x_int < 0 | x_int > 100] <- NA
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x_int <- ifelse(is.na(x_int), x_quali, x_int)
  }
  logMAR <- (-0.02 * x_int) + 1.7
  logMAR <- as.numeric(logMAR)
  class(logMAR) <- c("logmar", "va", class(logMAR))
  logMAR
}

#' @rdname va_methods
#' @param snellen which Snellen notation to display. Either "sn", "m" or "dec"
#' @description `toSnellen.etdrs`: Converting to logMAR with
#'   logMAR = -0.02 * ETDRS + 1.7, rounding this to the first digit,
#'   and matching the values to Snellen entries in [va_chart]
#' @export
toSnellen.etdrs <- function(x, snellen = "ft"){
  x_int <- suppressWarnings(as.integer(x))
  x_int[x_int < 0 | x_int > 100] <- NA
  if(any(x %in% unlist(set_codes()["quali"]))){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x_int <- ifelse(is.na(x_int), x_quali, x_int)
  }
  x <- round((-0.02 * x) + 1.7, 1)
  col = paste("snellen", snellen, sep = "_")
  Snellen <- va_chart[[col]][match(x, as.numeric(va_chart$logMAR))]
  class(Snellen) <- c("snellen", "va", class(Snellen))
  Snellen
}

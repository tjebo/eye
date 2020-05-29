
#' tologMAR.quali
#' @rdname va_methods
#' @param x vector of class quali
#' @family va conversion methods
tologMAR.quali <- function(x){
  logMAR <- va_quali$logMAR[match(x, va_quali$quali)]
  logMAR <- as.numeric(logMAR)
  class(logMAR) <- c("logmar","va",class(logMAR))
  logMAR
}
#' toSnellen.quali
#' @rdname va_methods
#' @param x vector of class quali
#' @family va conversion methods
toSnellen.quali <- function(x, snellen = "ft"){
  col = paste("snellen", snellen, sep = "_")
  Snellen <- va_chart[[col]][match(x_num, as.numeric(va_chart$quali))]
  class(Snellen) <- c("snellen","va",class(Snellen))
  logMAR
}
#' toETDRS.quali
#' @rdname va_methods
#' @param x vector of class quali
#' @family va conversion methods
toETDRS.quali <- function(x){
  ETDRS <- va_quali$ETDRS[match(x, va_quali$quali)]
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs","va",class(ETDRS))
  ETDRS
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
  logMAR <- as.numeric(logMAR)
  class(logMAR) <- c("logmar","va",class(logMAR))
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
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs","va", class(ETDRS))
  ETDRS
}

#' toSnellen.snellen
#' @rdname va_methods
#' @param x vector of class snellen
#' @family va conversion methods

toSnellen.snellen <- function(x, snellen = "ft"){
  col = paste("snellen", snellen, sep = "_")
  if(any(x %in% set_quali())){
    x_quali <- va_quali[[col]][match(x, va_quali$quali)]
    x <- ifelse(is.na(x), x_quali, x)
  }
  Snellen <- x
  class(Snellen) <- c("etdrs", "va", class(Snellen))
  Snellen
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
  x_num[x_num < -0.3 | x_num > 3] <- NA
  if(any(x %in% set_quali())){
    x_quali <- as.numeric(va_quali$logMAR[match(x, va_quali$quali)])
    x_num <- ifelse(is.na(x_num), x_quali, x_num)
  }
  col = paste("snellen", snellen, sep = "_")
  Snellen <- va_chart[[col]][match(x_num, as.numeric(va_chart$logMAR))]
  class(Snellen) <- c("snellen", "va", class(Snellen) )
  Snellen
}

#' toETDRS.logmar
#' @rdname va_methods
#' @param x vector of class logmar
#' @family va conversion methods
#' @details Only approximate eqivalent.
#'   Rounding logMAR to nearest first digit and
#'   converting to ETDRS letters using the va conversion chart
toETDRS.logmar <- function(x){
  x_num <- suppressWarnings(as.numeric(x))
  x_num[x_num < -0.3 | x_num > 3] <- NA
  if(any(x %in% set_quali())){
    x_quali <- va_quali$logMAR[match(x, va_quali$quali)]
    x_num <- ifelse(is.na(x_num), x_quali, x_num)
  }
  ETDRS <- eye::va_chart[["ETDRS"]][match(x_num , as.numeric(eye::va_chart[["logMAR"]]))]
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs", "va", class(ETDRS))
  ETDRS
}

#' tologMAR.logmar
#' @rdname va_methods
#' @param x vector of class logMAR
#' @family va conversion methods
#' @details will transform categories to logMAR
tologMAR.logmar <- function(x){
  x_num <- suppressWarnings(round(as.numeric(x), 2))
  x_num[x_num < -0.3 | x_num > 3] <- NA
  if(any(x %in% set_quali())){
    x_quali <- as.numeric(va_quali$logMAR)[match(x, va_quali$quali)]
    x_num <- ifelse(is.na(x_num), x_quali, x_num)
  }
  logMAR <- as.numeric(x_num)
  class(logMAR) <- c("logmar", "va", class(logMAR))
  logMAR
}


#' toETDRS.etdrs
#' @rdname va_methods
#' @param x vector of class etdrs
#' @family va conversion methods

toETDRS.etdrs <- function(x){
  x_int <- suppressWarnings(as.integer(x))
  x_int[x_int < 0 | x_int > 100] <- NA
  if(any(x %in% set_quali())){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x_int <- ifelse(is.na(x_int), x_quali, x_int)
  }
  ETDRS <- as.integer(ETDRS)
  class(ETDRS) <- c("etdrs", "va", class(ETDRS))
  ETDRS
}

#' tologMAR.etdrs
#' @rdname va_methods
#' @param x vector of class ETDRS
#' @family va conversion methods
#' @details Approximate eqivalent based on formula in Beck et al.
tologMAR.etdrs <- function(x){
  x_int <- suppressWarnings(as.integer(x))
  x_int[x_int < 0 | x_int > 100] <- NA
  if(any(x %in% set_quali())){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x_int <- ifelse(is.na(x_int), x_quali, x_int)
  }
  logMAR <- (-0.02 * x) + 1.7
  logMAR <- as.numeric(logMAR)
  class(logMAR) <- c("logmar", "va", class(logMAR))
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
  x_int <- suppressWarnings(as.integer(x))
  x_int[x_int < 0 | x_int > 100] <- NA
  if(any(x %in% set_quali())){
    x_quali <- va_quali$ETDRS[match(x, va_quali$quali)]
    x_int <- ifelse(is.na(x_int), x_quali, x_int)
  }
  x <- round((-0.02 * x) + 1.7, 1)
  col = paste("snellen", snellen, sep = "_")
  Snellen <- va_chart[[col]][match(x, as.numeric(va_chart$logMAR))]
  class(Snellen) <- c("snellen", "va", class(Snellen))
  Snellen
}

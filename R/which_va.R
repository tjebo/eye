#' Guessing the VA class
#' @name which_va
#' @param x Vector with VA entries
#' @param quali strings for qualitative visual acuity entries
#' @description Guessing the VA notation (VA class). Requires x that was
#' prepared with [clean_va]
#' * `which_va`: guessing VA class for entire vector
#' @return character vector indicating guessed VA notation
#' @keywords internal
#' @family VA helper
#' @family VA converter
#'
which_va <- function(x, quali = c("nlp", "npl", "pl", "lp", "hm", "cf")) {
  if (all(is.na(x))) {
    return("NA")
  }
  if (all(x[!is.na(x)] %in% quali)) {
    return("quali")
  }
  x_noquali <- x[!x %in% quali]
  x_num <- suppressWarnings(as.numeric(x_noquali))
  x_numval <- x_num[!is.na(x_num)]
  if (all(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("snellen")
  } else if (any(grepl("/", x_noquali[!is.na(x_noquali)])) & any(x_num <= 100)) {
    return(c("snellen", "logmar", "snellendec", "etdrs"))
  } else if (all(is.na(x_num))) {
    return("failed")
  }

  if (all(x_numval == as.integer(x_numval))) {
    if (all(x_numval > 3)) {
      return("etdrs")
    } else {
      return(c("etdrs", "logmar", "snellendec"))
    }
  } else if (any(x_numval == as.integer(x_numval))){
    return(c("logmar", "snellendec", "etdrs"))
  } else {
    return(c("logmar", "snellendec"))
  }
}
# x <-  c(0.2, 0.1, 75)
# which_va(x)
# which_va(snellen_logmar_na)
# which_va(etdrs_unplaus)

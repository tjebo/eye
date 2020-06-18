
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
which_va <- function(x, quali = c("nlp", "lp", "hm", "cf")) {
  if (all(is.na(x))) {
    return("NA")
  }
  if (all(x[!is.na(x)] %in% quali)) {
    return("quali")
  }
  x_noquali <- x[!x %in% quali]
  x_num <- suppressWarnings(as.numeric(x_noquali))

  if (all(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("snellen")
  } else if (any(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("mixed")
  } else if (all(is.na(x_num))) {
    return("failed")
  }

  x_numval <- x_num[!is.na(x_num)]
  if (length(x_numval) < length(x_noquali[!is.na(x_noquali)])) {
    return("mixed")
  }

  if (all(x_numval == as.integer(x_numval))) {
    if (all(x_numval %in% 0:3)) {
      return(c("logmar", "etdrs"))
    } else if (all(x_numval >= 0) & all(x_numval <= 100)) {
      return("etdrs")
    } else {
      return(c("etdrs", "implaus"))
    }
  }

  if (all(round(x_numval, 3) %in% as.numeric(va_chart$snellen_dec))) {
    return(c("logmar", "snellen"))
  } else if (any(x_numval < -0.3 )) {
    return(c("logmar", "implaus"))
  } else if(any(x_numval > 3)){
    x_bigint <- x_numval[x_numval > 3]
    if (all(x_bigint== as.integer(x_bigint))){
      return(c("logmar", "etdrs"))
    } else {
      return(c("logmar", "implaus"))
    }
    } else {
    return("logmar")
  }
}

#' @rdname which_va
#' @param elem element of vector to convert
#' @param from_logmar chose logmar when guessing between two notations
#' @description * `which_va_dissect`: guessing VA class for each vector element
which_va_dissect <- function(elem, from_logmar) {
  guess_va <- which_va(elem)

  if (length(guess_va) == 2) {
    if (any(guess_va %in% "implaus")) {
      warning("NA introduced (implausible values)", call. = FALSE)
    }
    if(isTRUE(from_logmar)){
      from <- "logmar"
    } else {
      from <- guess_va[!guess_va %in% "logmar"]
    }
    class_va <- from
  } else if (guess_va == "failed") {
    warning("NA introduced (character only). See ?va for help",
            call. = FALSE
    )
    class_va <- "NA"
  } else {
    class_va <- guess_va
  }
  class_va
}

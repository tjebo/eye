
#' Guessing the VA class
#' @name which_va
#' @param x Vector with VA entries
#' @description Guessing the VA notation (VA class)
#' * `which_va`: guessing VA class for entire vector
#' @family VA helper
#' @family VA converter
#'
which_va <- function(x) {
  x <- tolower(suppressWarnings(as.character(x)))
  if (all(is.na(x))) {
    return("NA")
  }
  if (all(x[!is.na(x)] %in% unlist(set_codes()["quali"]))) {
    return("quali")
  }
  x_noquali <- x[!x %in% unlist(set_codes()["quali"])]
  x_num <- suppressWarnings(as.numeric(x_noquali))
  if (all(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("snellen")
  } else if (any(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("mixed")
  }

  if (all(is.na(x_num))) {
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

  if (all(round(x_num[!is.na(x_num)], 2) %in% inter_snelllog)) {
    return(c("logmar", "snellen"))
  } else if (all(round(x_num, 3) %in% as.numeric(va_chart$snellen_dec))) {
    return("snellen")
  } else if (any(x_numval < -0.3 | any(x_numval > 3))) {
    return(c("logmar", "implaus"))
  } else {
    return("logmar")
  }
}

#' @rdname which_va
#' @param elem element of vector to convert
#' @description * `which_va_dissect`: guessing VA class for each vector element
which_va_dissect <- function(elem) {
  guess_va <- which_va(elem)

  if (length(guess_va) == 2) {
    if (any(guess_va %in% "implaus")) {
      warning("NA introduced (implausible values)", call. = FALSE)
    }
    guess_va <- "logmar"
    class_va <- "logmar"
  } else if (guess_va == "failed") {
    warning("NA introduced (character only). See ?va for help",
            call. = FALSE
    )
    elem <- NA
    class_va <- "logmar"
  } else {
    class_va <- guess_va
  }
  class_va
}

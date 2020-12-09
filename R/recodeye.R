#' Recode eyes
#' @description recoding eyes to "r" and "l"
#' @name recodeye
#' @param x vector of strings
#' @param to named vector to which eye codes. If unnamed, this order: c(r, l, b)
#' @param eyecodes named list of substrings which should be converted to
#' right and left eyes - if passed unnamed, this order: list(r, l, b)
#' @param numcode specify if you have numeric coding which is not 0:1 or 1:2
#' @param dropunknown introduces NA for values that are not part of eyecodes
#' @section string detection:
#' recodeye will automatically detect the following strings:
#'   right = c("r", "re", "od", "right"),
#'   left = c("l", "le", "os", "left"),
#'   both = c("b","both","ou")
#' @section to and eyecode arguments:
#' If passed, should ideally be of same length, and have the respective eyes
#' at the same index (or with the same name!). If the lengths are not equal,
#' e.g., if only "to" is passed with n elements, the shorter argument will be
#' will be cut down to the first n elements of the longer argument.
#'
#' Note that all unique strings which are part of the column should be contained
#' in the "eyecode" argument.
#' @section numeric coding:
#' Currently numeric coding only accepts binary
#' coding (right and left eye). In order to use numeric coding for "both eyes"
#' as well, a workaround using the eyecodes argument is suggested.
#' @return Character vector
#' @family string matching functions
#' @examples
#' x <- c("r", "re", "od", "right", "l", "le", "os", "left")
#' recodeye(x)
#' ## chose the resulting codes
#' recodeye(x, to = c("right", "left"))
#' x <- 1:2
#' recodeye(x)
#' ## or, if right is coded with 2)
#' recodeye(x, numcode = 2:1)
#'
#' @export
recodeye <- function(x, to = NULL,
                     eyecodes = NULL,
                     numcode = NULL,
                     dropunknown = TRUE) {
  if(!is.atomic(x)) {
    stop("x needs to be atomic", call. = FALSE)
  }
  if (is.null(to)) {
    to <- c(r = "r", l = "l", b = "b")
  } else {
    if (is.null(names(to))) {
      names(to) <- c("r", "l", "b")[1:length(to)]
    }
  }

  if (!is.null(eyecodes)) {
    if (!inherits(eyecodes, "list")) {
      stop("eyecodes needs to be list", call. = FALSE)
    }
    if (is.null(names(eyecodes))) {
      names(eyecodes) <- c("r", "l", "b")[1:length(eyecodes)]
    }
  } else {
    eyecodes <- list(
      r = c("r", "re", "od", "right"),
      l = c("l", "le", "os", "left"),
      b = c("b", "both", "ou")
    )
  }

  if (length(eyecodes) > length(to)) {
    eyecodes <- eyecodes[1:length(to)]
  } else if (length(eyecodes) < length(to)) {
    to <- to[1:length(eyecodes)]
  }

  names(eyecodes) <- to[match(names(eyecodes), names(to))]

  x[isNAstring(x)] <- NA
  if (sum(is.na(x) > 0)) {
    message("Not all eyes are identified (contains NA)")
  }
  eyeInt <- suppressWarnings(unique(as.integer(x[!is.na(x)])))
  eyeInt_noNA <- eyeInt[!is.na(eyeInt)]
  eyeChar <- as.logical(sum(is.na(eyeInt)))

  if (!(is.null(numcode) | eyeChar)) {
    if (length(numcode) != 2) {
      stop("numcode needs to be two numerics only", call. = FALSE)
    }
    if (!all(eyeInt_noNA %in% numcode)) {
      warn_recode(x)
      return(x)
    } else {
      message(paste0(
        "Eyes coded ", paste(numcode, collapse = ":"),
        " with r = ", numcode[1]
      ))
      x[x == numcode[1]] <- to[1]
      x[x == numcode[2]] <- to[2]
      return(x)
    }
  } else if (eyeChar) {
    if (length(eyeInt_noNA > 0)) {
      if (!dropunknown) {
        warn_recode(x)
        return(x)
      } else {
        warn_NA(x, eyecodes)
        x[!(x %in% unique(unname(unlist(eyecodes))))] <- NA_character_
        return(x)
      }
    } else if (!all(tolower(unique(x)) %in% c(NA, unname(unlist(eyecodes))))) {
      warn_recode(x)
      if (!dropunknown) {
        warn_recode(x)
        return(x)
      } else {
        x[!x %in% unique(unname(unlist(eyecodes)))] <- NA
        warn_NA(x, eyecodes)
      }
    }
  } else if (length(eyeInt_noNA) > 2 | length(eyeInt_noNA) == 1) {
    warn_recode(x)
    return(x)
  } else if (length(eyeInt_noNA) == 2) {
    if (!(all(eyeInt_noNA %in% 0:1) | all(eyeInt_noNA %in% 1:2))) {
      warn_recode(x)
      return(x)
    } else {
      if (all(eyeInt_noNA %in% 0:1)) {
        message("Eyes coded 0:1. Interpreting r = 0")
        recode_lookup <- c("0" = to[1], "1" = to[2])
        x <- unname(recode_lookup[as.character(x)])
        return(x)
      } else if (all(eyeInt_noNA %in% 1:2)) {
        message("Eyes coded 1:2. Interpreting r = 1")
        recode_lookup <- c("1" = to[1], "2" = to[2])
        x <- unname(recode_lookup[as.character(x)])
        return(x)
      }
    }
  } else if (eyeChar) {
    if (!all(tolower(unique(x)) %in% c(NA, unname(unlist(eyecodes))))) {
      warn_recode(x)
      if (!dropunknown) {
        warn_recode(x)
        return(x)
      } else {
        x[!x %in% unique(unname(unlist(eyecodes)))] <- NA
        warn_NA(x, eyecodes)
      }
    }
  }

  lookups <- data.frame(
    match = rep(names(eyecodes), lengths(eyecodes)),
    token = unlist(eyecodes)
  )
  match_lookups <- lookups$match[match(tolower(x), lookups$token)]
  match_lookups
}

#' recode warnings
#' @name warnings
#' @keywords internal
warn_recode <- function(x){
  warning(paste("Unclear eye coding! Your values:",
                paste(unique(x), collapse = ", "),
                "\nCheck ?recodeye how to specify coding."),
          call. = FALSE)
}

#' @rdname warnings
#' @keywords internal
warn_NA <- function(x, eyecodes){
  warning(paste("Introduced NA for unclear values:",
                paste(unique(x[!x %in% c(NA_character_, unique(unname(unlist(eyecodes))))]),
                      collapse = ", ")),
          call. = FALSE)
}


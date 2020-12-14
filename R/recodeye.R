#' Recode eyes
#' @description recoding eyes to "r" and "l"
#' @name recodeye
#' @param x vector of strings
#' @param to named vector to which eye codes. If unnamed, this order: c(r, l, b)
#' @param eyecodes named list of substrings which should be converted to
#' right and left eyes - if passed unnamed, this order: list(r, l, b)
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
#'
#' ## or with "both eyes"
#' x <- c(x, "both", "ou")
#' recodeye(x)
#'
#' ## chose the resulting codes
#' recodeye(x, to = c("od", "os", "ou"))
#'
#' x <- 1:2
#' recodeye(x)
#' @export
recodeye <- function(x, to = NULL,
                     eyecodes = NULL,
                     dropunknown = TRUE) {
  if (!is.atomic(x)) stop("x needs to be atomic", call. = FALSE)

  if (is.null(to)) {
    to <- c(r = "r", l = "l", b = "b")
  } else if (is.null(names(to))) {
    names(to) <- c("r", "l", "b")[1:length(to)]
  }
  to_r <- unname(to[names(to) == "r"])
  to_l <- unname(to[names(to) == "l"])

  if (is.null(eyecodes)) {
    eyecodes <- list(
      r = c("r", "re", "od", "right"),
      l = c("l", "le", "os", "left"),
      b = c("b", "both", "ou")
    )
  } else if (!inherits(eyecodes, "list")) {
    stop("eyecodes needs to be list")
  } else if (is.null(names(eyecodes))) {
    names(eyecodes) <- c("r", "l", "b")[1:length(eyecodes)]
  }

  if (length(eyecodes) > length(to)) {
    eyecodes <- eyecodes[1:length(to)]
  } else if (length(eyecodes) < length(to)) {
    to <- to[1:length(eyecodes)]
  }

  names(eyecodes) <- to[match(names(eyecodes), names(to))]

  x <- tidyNA(x)
  if (sum(is.na(x) > 0)) {
    message("Missing values and/or meaningless strings contained")
  }
  eyeInt <- suppressWarnings(unique(as.integer(x[!is.na(x)])))
  eyeInt_noNA <- eyeInt[!is.na(eyeInt)]
  eyeChar <- as.logical(sum(is.na(eyeInt)))

  if (eyeChar) {
    if (!all(tolower(unique(x)) %in% c(NA, unname(unlist(eyecodes))))) {
      if (!dropunknown) {
        warn_recode(x)
        return(x)
      } else {
        messageNA(x, eyecodes)
        x[!(x %in% unique(unname(unlist(eyecodes))))] <- NA_character_
      }
    }
  } else if (!(all(eyeInt_noNA %in% 0:1) | all(eyeInt_noNA %in% 1:2))) {
    warn_recode(x)
    return(x)
  } else {
    if (all(eyeInt_noNA %in% 0:1)) {
      message("Eyes coded 0:1. Interpreting r = 0")
      recode_lookup <- c("0" = to_r, "1" = to_l)
      x <- unname(recode_lookup[as.character(x)])
      return(x)
    } else if (all(eyeInt_noNA %in% 1:2)) {
      message("Eyes coded 1:2. Interpreting r = 1")
      recode_lookup <- c("1" = to_r, "2" = to_l)
      x <- unname(recode_lookup[as.character(x)])
      return(x)
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
  warning(paste("NOT RECODED - unclear eye coding! Your values:",
                paste(unique(x), collapse = ", "),
                "\nCheck ?recodeye how to specify coding."),
          call. = FALSE)
  }

#' @rdname warnings
#' @keywords internal
messageNA <- function(x, eyecodes){
  message(paste("Introduced NA for unclear values:",
                paste(unique(x[!x %in% c(NA_character_, unique(unname(unlist(eyecodes))))]),
                      collapse = ", ")))
  }


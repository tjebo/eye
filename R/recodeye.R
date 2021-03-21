#' Recode eyes
#' @description recoding eyes to "r" and "l"
#' @name recodeye
#' @param x vector of strings
#' @param to named vector to which eye codes. If unnamed, this order: c(r, l, b)
#' @param eyestrings named list of substrings which should be converted to
#' right and left eyes - if passed unnamed, this order: list(r, l, b)
#' @param dropunknown introduces NA for values that are not part of eyestrings
#' @section string detection:
#' recodeye will automatically detect the following strings:
#'   right = c("r", "re", "od", "right"),
#'   left = c("l", "le", "os", "left"),
#'   both = c("b","both","ou")
#'
#'   You can change this with [set_eye_strings]
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
#' as well, a workaround using the eyestrings argument is suggested.
#' @return Character vector
#' @family string matching functions
#' @examples
#' x <- c("r", "re", "od", "right", "l", "le", "os", "left", "both", "ou")
#' recodeye(x)
#'
#' ## chose the resulting codes
#' recodeye(x, to = c("od", "os", "ou"))
#'
#' x <- 1:2
#' recodeye(x)
#'
#' ## If you code your eyes with different strings,
#' ## e.g., because you are using a different language,
#' ## you can change this either with the eyestrings argument,
#' french <- c("OD", "droit", "gauche", "OG")
#' recodeye(french, eyestrings = list(r = c("droit", "od"), l = c("gauche", "og")))
#'
#' ## or change it more globally with `set_eye_strings`
#' set_eye_strings(right = c("droit", "od"), left = c("gauche", "og"))
#' recodeye(french)
#'
#' ## restore defaults with
#' set_eye_strings()
#' @importFrom stats setNames
#' @export
recodeye <- function(x, to = NULL,
                     eyestrings = NULL,
                     dropunknown = TRUE) {
  if (!is.atomic(x)) stop("x needs to be atomic", call. = FALSE)

  if (is.null(to)) {
    to <- c(r = "r", l = "l", b = "b")
  } else if (is.null(names(to))) {
    names(to) <- c("r", "l", "b")[1:length(to)]
  }
  to_r <- unname(to[names(to) == "r"])
  to_l <- unname(to[names(to) == "l"])

  if (is.null(eyestrings)) {
    eyestrings <-
      stats::setNames(eye_codes[c("right", "left", "both")], c("r", "l", "b"))
  } else if (!inherits(eyestrings, "list")) {
    stop("eyestrings needs to be list")
  } else if (is.null(names(eyestrings))) {
    names(eyestrings) <- c("r", "l", "b")[1:length(eyestrings)]
  }

  if (length(eyestrings) > length(to)) {
    eyestrings <- eyestrings[1:length(to)]
  } else if (length(eyestrings) < length(to)) {
    to <- to[1:length(eyestrings)]
  }

  names(eyestrings) <- to[match(names(eyestrings), names(to))]

  x <- tidyNA(tolower(x))
  x <- gsub("^\\s+|\\s+$", "", x)

  if (sum(is.na(x) > 0)) {
    message("Missing values and/or meaningless strings contained")
  }
  eyeInt <- suppressWarnings(unique(as.integer(x[!is.na(x)])))
  eyeInt_noNA <- eyeInt[!is.na(eyeInt)]
  eyeChar <- as.logical(sum(is.na(eyeInt)))
  if (eyeChar) {
    if (!all(unique(x) %in% c(NA, unname(unlist(eyestrings))))) {
      if (!dropunknown) {
        warn_recode(x)
        return(x)
      } else {
        messageNA(x, eyestrings)
        x[!(x %in% unique(unname(unlist(eyestrings))))] <- NA_character_
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
    match = rep(names(eyestrings), lengths(eyestrings)),
    token = unlist(eyestrings)
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
messageNA <- function(x, eyestrings){
  message(paste("Introduced NA for unclear values:",
                paste(unique(x[!x %in% c(NA_character_, unique(unname(unlist(eyestrings))))]),
                      collapse = ", ")))
  }


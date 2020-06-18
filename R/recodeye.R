#' Recode eyes
#' @description recoding eyes to "r" and "l"
#' @name recodeye
#' @param x vector of strings
#' @param to to which codes. Right coded first.
#' @param eyecodes list of substrings which should be converted to
#' right and left eyes - first vector for right, then for left eyes
#' @param numcode if you have numeric coding which is not 0:1 or 1:2 for
#' right:left, specify it here.
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
recodeye <- function(x, to = c("r", "l"),
                        eyecodes = list(
                          c("r", "re", "od", "right"),
                          c("l", "le", "os", "left")
                        ),
                        numcode = NULL) {
  if (!inherits(eyecodes, "list")) {
    stop("eyecodes needs to be list", call. = FALSE)
  }
  names(eyecodes) <- to

  x[isNAstring(x)] <- NA
  eyeInt <- suppressWarnings(unique(as.integer(x[!is.na(x)])))
  eyeInt_noNA <- eyeInt[!is.na(eyeInt)]
  eyeChar <- as.logical(sum(is.na(eyeInt)))

  if (!is.null(numcode)) {
    if (length(numcode) != 2) {
      stop("numcode needs to be two numerics", call. = FALSE)
    }
    if (!all(eyeInt_noNA %in% numcode)) {
      warning(paste("Unclear eye coding! Not recoded. Please clean data.
            Your values:", paste(unique(x), collapse = ", ")), call. = FALSE)
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
  }

  if (eyeChar & length(eyeInt_noNA > 0)) {
    warning(paste("Unclear eye coding! Not recoded. Please clean data.
            Your values:", paste(unique(x), collapse = ", ")), call. = FALSE)
    return(x)
  }
  if (length(eyeInt_noNA) > 2 | length(eyeInt_noNA) == 1) {
    warning(paste("Unclear eye coding! Not recoded. Please clean data.
            Your values:", paste(unique(x), collapse = ", ")), call. = FALSE)
    return(x)
  } else if (length(eyeInt_noNA) == 2) {
    if (!(all(eyeInt_noNA %in% 0:1) | all(eyeInt_noNA %in% 1:2))) {
      warning(paste("Unclear eye coding! Not recoded. Please clean data.
            Your values:", paste(unique(x), collapse = ", ")), call. = FALSE)
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
      warning(paste("Unclear eye coding! Not recoded. Please clean data.
            Your values:", paste(unique(x), collapse = ", ")), call. = FALSE)
      return(x)
    }
    lookups <- data.frame(
      match = rep(names(eyecodes), lengths(eyecodes)),
      token = unlist(eyecodes)
    )
    match_lookups <- lookups$match[match(tolower(x), lookups$token)]
    match_lookups
  }
}


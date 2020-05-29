#' Count patients and eyes
#' @name eyes
#' @description Counts number of patients and eyes (right and left).
#' @param \strong{x} required. (data frame)
#' @param id Patient identifying column
#' @param eye Eye identifying colum.
#' @param report if TRUE, string returned to paste into report
#' @param ... passed to [eyes_to_string]
#' @details
#' `eyes` guesses columns that identify patients and eyes.
#' - use *id* if `eyes` fails to identify the patient variable.
#'   `eyes` searches for column names containing the strings "id" and/or "pat"
#'   (any cases). Column names that contain both strings are prioritised.
#'   If more than one column is found, the function will return an error.
#' - use *eye* if `eyes` fails to identify the eye column.
#'   `eyes` searches for column names containing the string "eye" (any cases).
#' - `eyes` recognizes integer coding 0:1 and 1:2, with right being
#'    the lower number. For strings coding it recognizes
#'    right eyes: c("r", "re", "od", "right") and
#'    left eyes: c("l", "le", "os", "left")
#' - *report* and **...** : [eyes_to_string] understands the following params:
#'     - **english** If TRUE: writing numbers <= 12 as words
#'     - **para** If TRUE: Adding "A total of" to comply with most
#'     journal standards and to avoid awkward long numnbers.
#'     - **UK** Logical, Use UK (English) style (TRUE) or
#'     USA (American) style (FALSE).
#' @return Vector with count of patients and eyes
#' @family eye core functions
#' @family Eye count functions
#' @examples
#' eyes(amd)
#' @export
#'
eyes <- function(x, id  = NULL, eye  = NULL, report = FALSE, ...) {

  if (is.null(id)) {
    pat_col <- get_idcols(x)
  } else {
    pat_col <- id
  }

  if (length(pat_col) < 1) {
    stop("Patient column missing. Fix with \"id\" argument", call. = FALSE)
  }

  if (is.null(eye)) {
    eye <- part_str("eye")(colnames(x))
  }

  if (length(eye) > 1 | length(pat_col) > 1) {
    stop("Could not identify patient or eye column - fix with \"id\" or \"eye\" argument", call. = FALSE)
  }

  n_pat <- length(unique(x[[pat_col]]))

  if (length(eye) < 1 | identical(eye, character(0))) {
    message("No eye column found: Only patients counted.")
    if(isTRUE(report)){
      eyes_to_string(n_pat, ...)
    } else {
    return(c(patients = n_pat))
}
  } else if (length(eye) == 1) {

    eye_str <- set_eye()

    eye_int <- suppressWarnings(unique(as.integer(x[[eye]])))
    eye_integ <- eye_int[!is.na(eye_int)]
    eye_char <- as.logical(sum(is.na(eye_int)))

    if (length(eye_integ) > 2 | length(eye_integ) == 1) {
      stop("Eye coding ambiguous - guessing failed! Please clean data", call. = FALSE)
    } else if (length(eye_integ) == 2) {

      if (!(all(eye_integ %in% 0:1) | all(eye_integ %in% 1:2))) {
        stop("Eye coding ambiguous - guessing failed! Please clean data", call. = FALSE)
      } else {
        if(all(eye_integ %in% 0:1)){
          message("Eyes coded 0:1. Interpreting as r = 0")
        } else if(all(eye_integ %in% 1:2)){
          message("Eyes coded 1:2. Interpreting as r = 1")
        }
        if(eye_char){
          warning("Eye coding ambiguous - characters omitted", call. = FALSE )
        }
      }
    } else if (eye_char) {
      if (!all(tolower(unique(x[[eye]])) %in% c(NA, unname(unlist(eye_str))))) {
        stop("Eye coding ambiguous - guessing failed! Please clean data", call. = FALSE)
      }
    }
    if (sum(is.na(x[[eye]]) > 0)) {
      message("Not all eyes are identified (contains NA)")
    }
    eye_tab <- table(unique(x[, c(pat_col, eye)]))
    if (sum(is.na(eye_int) == 0)) {
      n_r <- unname(colSums(eye_tab[, 1, drop = FALSE]))
      n_l <- unname(colSums(eye_tab[, 2, drop = FALSE]))
    } else {
      tab_r <- eye_tab[, tolower(colnames(eye_tab)) %in% eye_str$r, drop = FALSE]
      tab_l <- eye_tab[, tolower(colnames(eye_tab)) %in% eye_str$l, drop = FALSE]

      if (ncol(tab_r) > 1 | ncol(tab_l) > 1) {
        paste_fac <- function(x) {
          paste(x, collapse = ",")
        }
        message("Eye coding somewhat messy - recommend cleaning"
        )
      }
      n_r <- sum(colSums(tab_r))
      n_l <- sum(colSums(tab_l))
    }
    n_eyes <- n_r + n_l
    if(isTRUE(report)){
      eyes_to_string(c(n_pat, n_eyes), ...)
    } else {
    return(c(patients = n_pat, eyes = n_eyes, right = n_r, left = n_l))
    }
  }
}

#' counteyes
#' @rdname eyes
#' @export
counteyes <- eyes

#' count_eyes
#' @rdname eyes
#' @export
count_eyes <- eyes

#' Eye count to string
#' @rdname eyes
#' @description `eyestr`: identical to `eyes(x, report = TRUE, ...)`
#' @return `eyestr`: String to paste into reports
#' @family Eye count functions
#' @examples
#' eyestr(amd, para = TRUE)
#' @export
eyestr <- function(x, id = NULL, eye = NULL, english = TRUE, para = FALSE, UK = FALSE){
suppressMessages(eyes(x = x, id = id, report = TRUE, eye = eye, english = english, para = para, UK = UK))
}

#' Eye count to strings
#' @name eyes_to_string
#' @param x vector of one or two
#' @param english If TRUE: writing numbers <= 12 as words
#' @param para If TRUE: Adding "A total of" to comply with most
#' journal standards and to avoid awkward long numnbers.
#' @param UK Logical, Use UK (English) style (TRUE) or
#'   USA (American) style (FALSE).
#' @return String to paste into text
#' @family Eye count functions
#' @importFrom english english

eyes_to_string <- function(x, english = TRUE, para = FALSE, UK = FALSE) {
  if (para) {
    para <- "A total of "
  } else {
    para <- NULL
  }
  if (isTRUE(english)) {
    engl <- as.character(english::english(x[x <= 12], UK = UK))
    x[x <= 12] <- engl
  }
  if (x[1] <= 1) {
    patient <- "patient"
  } else {
    patient <- "patients"
  }
  if (length(x) == 1) {
    return(paste0(para, paste(tocapital(x[1]), patient)))
  } else {
    if (x[2] <= 1) {
      eye <- "eye"
    } else {
      eye <- "eyes"
    }
    return(paste0(para, paste(tocapital(x[2]),
                              eye, "of", x[1], patient)))
  }
}

#' eyes
#' @name eyes
#' @description Looks for columns that identify patients and eyes and counts number of patients and eyes.
#' @param \strong{x} required. (data.frame)
#' @param id Use if `eyes` does not find an id / patient column. See details id column.
#' @param eye Use if `eyes` does not find an eye colum. See details
#' @param eye_str Use not recommended - passed to name searching functions (details)
#' @details
#' - **id** specify if `eyes` fails to identify the patient columns .
#'   `eyes` searches for column names that contain the string "id" or "pat"
#'   (any letter cases). Column names that contain both strings are prioritised.
#'   If more than one column is found, the function will return an error.
#'   This could cause potential confusion and then the argument identifies which column to pick.
#' - **eye** `eyes` is looking for column names that contain the string "eye" (any letter cases).
#'   This could cause potential confusion and then the argument identifies which column to pick.
#' - **eye_str**: for strings coding eyes: named list of character vectors
#'    coding right and left eyes. names need to be "r" and "l".
#'    Default `list(r = c("r", "re", "od", "right"),  l = c("l", "le", "os", "left"))`
#' @family ophthalmic functions
#' @examples
#' eyes(amd)
#' @export
#'
eyes <- function(x, id  = NULL, eye  = NULL, eye_str = NULL) {

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
    return(c(patients = n_pat))

  } else if (length(eye) == 1) {
    if(is.null(eye_str)){
      eye_str <- set_eye()
    } else if(!inherits(eye_str, "list") | !identical(names(eye_str), c("r", "l"))) {
      stop("eye_str needs to be list with c(\"r\",\"l\") as names", call. = FALSE)
    }
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
    return(c(patients = n_pat, eyes = n_eyes, right = n_r, left = n_l))
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

#' insight
#' @name insight
#' @description wrapper around eyes, returning text for document
#' @param x data frame
#' @param ... passed to [eyes]
#' @family ophthalmic functions
#' @examples
#' insight(amd)
#' @export

insight <- function(x, ...) {
  counts <- eye::eyes(x, ...)
  if (counts[1] <= 1) {
    patient <- "patient"
  } else {
    patient <- "patients"
  }
  if (length(counts) == 1) {
    return(paste(counts[1], patient))
  } else {
    if (counts[2] <= 1) {
      eye <- "eye"
    } else {
      eye <- "eyes"
    }
    return(paste(counts[2], eye, "of", counts[1], patient))
  }
}

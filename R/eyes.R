#' eyes
#' @name eyes
#' @description Looks for columns that identify patients and eyes and counts number of patients and eyes.
#' @param date dataframe where patient / eye information is included.
#' @param id id column. If not specified, automatically selected. In this case, the patient ID column name needs to contain the strings "pat" OR "id" ignore.case = TRUE.
#' @param eye eye colum. If not specified, automatically selected, in which case the eye column name needs to contain the string "eye" ignore.case = TRUE
#' @param text default = FALSE (TRUE will return text which can be pasted for example into a markdown document). As default a named vector will be returned
#' @examples
#' eyes(amd)
#' @export
#'
eyes <- function(data, id = NULL, eye = NULL, text = FALSE) {
  x <- eval(data)

  if (missing(id)) {
    pat_col <- names(x)[grepl("pat|id", names(x), ignore.case = TRUE)]
  } else {
    pat_col <- id
  }

  if (missing(eye)) {
    eye_col <- names(x)[grepl("eye", names(x), ignore.case = TRUE, perl = TRUE)]
  } else {
    eye_col <- eye
  }

  if (length(pat_col) < 1) {
    stop("Patient column missing. Fix with \"id\" argument", call. = FALSE)
  }

  if (length(eye_col) > 1 | length(pat_col) > 1) {
    stop("Could not identify patient or eye column - fix with \"id\" or \"eye\" argument", call. = FALSE)
  }

  n_pat <- length(unique(x[[pat_col]]))

  if (length(eye_col) < 1 | identical(eye_col, character(0))) {
    message("No eye column found: Only patients counted.")
    return(c(patients = n_pat))

  } else if (length(eye_col) == 1) {
    eye_int <- suppressWarnings(unique(as.integer(x[[eye_col]])))
    eye_integ <- eye_int[!is.na(eye_int)]
    eye_char <- as.logical(sum(is.na(eye_int)))

    if (length(eye_integ) > 2 | length(eye_integ) == 1) {
      stop("Eye coding ambiguous - guessing failed! Please clean data", call. = FALSE)
    } else if (length(eye_integ) == 2) {
      # return(eye_int)
      if (!(all(eye_integ %in% 0:1) | all(eye_integ %in% 1:2))) {
        stop("Eye coding ambiguous - guessing failed! Please clean data", call. = FALSE)
      } else {
        message("Eyes are coded 0:1 or 1:2. Interpreting as r:l")
        if(eye_char){
          warning("Eye coding ambiguous - characters omitted", call. = FALSE )
        }
        }
        } else if (eye_char) {
      if (!all(tolower(unique(x[[eye_col]])) %in% c(NA, "r", "l", "re", "le", "od", "os"))) {
        stop("Eye coding ambiguous - guessing failed! Please clean data", call. = FALSE)
      }
    }
    if (sum(is.na(x[[eye_col]]) > 0)) {
      message("Not all eyes are identified (contains NA)")
    }
    eye_tab <- table(unique(x[, c(pat_col, eye_col)]))
    if (sum(is.na(eye_int) == 0)) {
      n_r <- unname(colSums(eye_tab[, 1, drop = FALSE]))
      n_l <- unname(colSums(eye_tab[, 2, drop = FALSE]))
    } else {
      tab_r <- eye_tab[, tolower(colnames(eye_tab)) %in% c("r", "re", "od"), drop = FALSE]
      tab_l <- eye_tab[, tolower(colnames(eye_tab)) %in% c("l", "le", "os"), drop = FALSE]

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

    if (text) {
      return(paste(n_eyes, "eyes of", n_pat, "patients"))
    }
    return(c(patients = n_pat, eyes = n_eyes, right = n_r, left = n_l))
  }
}

#' count_eyes
#' @rdname eyes
#' @export
counteyes <- eyes

#' count_eyes
#' @rdname eyes
#' @export
count_eyes <- eyes

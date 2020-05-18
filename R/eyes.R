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
    stop("Could not identify patient or eye column -
         there seem to be more than one variable containing the strings \"eye\" or \"id\".
         Fix with the id or eye argument", call. = FALSE)
  }

  n_pat <- length(unique(x[[pat_col]]))

  if (length(eye_col) < 1 | identical(eye_col, character(0))) {
    message("No eye column found in data: Only patients counted.")
    return(c(patients = n_pat))
  } else if (length(eye_col) == 1) {
    eye_int <- suppressWarnings(unique(as.integer(x[[eye_col]])))

    if (length(eye_int) > 2) {
      stop("There are more than 2 numeric codes for eyes. Clean your data", call. = FALSE)
    } else if (length(eye_int) == 2) {
      if (!(all(eye_int %in% 0:1) | all(eye_int %in% 1:2))) {
        stop("Eyes are numerically coded, but coding is not 0/1 or 1/2.
           Please change the codes for eyes.", call. = FALSE)
      } else {
        message("Eyes are numerically coded. Interpreting r = 0 or 1, respectively")
      }
    } else if (is.na(eye_col)) {
      if (!all(tolower(unique(x[[eye_col]])) %in% c(NA, "r", "l", "re", "le", "od", "os"))) {
        stop("Eyes not coded clearly.
          Must be either integers 0/1 or 1/2 or characters c(\"r\", \"l\", \"re\", \"le\", \"od\", \"os\") -
             any cases allowed!", call. = FALSE)
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
        warning("Eye coding ambiguous - guessing! Recommend to clean data", call. = FALSE
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

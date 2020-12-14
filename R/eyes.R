#' Count patients and eyes
#' @name eyes
#' @description Counts number of patients and right and left eyes. Columns are
#'     guessed.
#' @param x required. (data frame)
#' @param id Patient identifying column, passed as (quoted) character
#' @param eye Eye identifying column, passed as (quoted) character
#' @param dropunknown introduces NA for values not recognized by [recodeye]
#' @section Guessing:
#' **id** and **eye** arguments overrule the name guessing for
#' the respective columns (here, cases need to match)
#'
#' For any below, **cases are always ignored** (column names can be in upper or
#'    lower case, as you please)
#'
#' **patient ID columns**:
#' - First, `eyes` is looking for names that contain both strings
#' "pat" and "id" (the order doesn't matter)
#' - Next, it will look for columns that are plainly called "ID"
#' - Last, it will search for all names that contain either "pat"
#' or "id"
#'
#' **eye variable column**:
#' - `eyes` primarily looks for columns called either "eye" or "eyes",
#' and if they are not present, columns containing string "eye"
#' (e.g., EyeName will be recognized)
#'
#' @section Eye coding:
#' The following codes are recognized:
#'
#' - integer coding 0:1 and 1:2, right eye being the lower number.
#' - right eyes: c("r", "re", "od", "right") and
#' - left eyes: c("l", "le", "os", "left") and
#' - both eyes: c("b", "both", "ou")
#'
#' If your eye column contains other values, they will be dropped to
#' NA (dropunknown) or kept (and then only patients will be counted, because
#' coding remains unclear). Recommend then to recode with [recodeye]
#' @section eyestr:
#'  eyestr is a wrapper around eyes. It parses the result into meaningful text,
#'  which can be #'  readily pasted into reports with [eyes_to_string] under the hood.
#'  Arguments to `eyes_to_string` are passed via **...**:
#'  - **small_num** If TRUE (default): numbers <= 12 as words
#'  - **para** If TRUE (not default): Adding "A total of" to
#'     comply with most journal standards and to avoid awkward
#'     long numbers.
#'  - **UK** TRUE: UK style (English) or FALSE (default):
#'  US style (American).
#' @return `eyes`: Named integer vector with count of patients and eyes
#' @family eye core functions
#' @importFrom purrr quietly
#' @examples
#' library(eyedata)
#' eyes(amd2)
#' @export
#'
eyes <- function(x, id = NULL, eye = NULL, dropunknown = TRUE) {
  if (!inherits(x, "data.frame")) {
    stop("x must be a data frame", call. = FALSE)
  }
  if (nrow(x) < 1) {
    return("0 eyes of 0 patients")
  }
  if (is.null(id)) {
    pat_col <- getElem_id(x)
  }

  if (length(pat_col) != 1) {
    warning("Did not find the ID column - use argument \"id\"",
      call. = FALSE
    )
    return(NULL)
  }
  if (is.null(eye)) {
    eye <- getElem_eyecol(x)
  }

  if (length(eye) > 1) {
    warning("Please define eye column", call. = FALSE)
    return(NULL)
  }
  if (length(eye) < 1) {
    message("No eye column found: Counting patients only")
    res <- c(patients = length(unique(x[[pat_col]])))
  } else if (length(eye) == 1) {
    quiet_recode <- purrr::quietly(~ recodeye(x = .x, dropunknown = .y))
    recode_eye <- quiet_recode(.x = x[[eye]], .y = dropunknown)
    if (length(recode_eye$warnings) > 0) {
      message(recode_eye$warnings)
      return(c(patients = length(unique(x[[pat_col]]))))
    }
    if (length(recode_eye$messages) > 0) {
      message(gsub("\\\n", "", recode_eye$messages))
    }
    if(any(unique(recode_eye$result) %in% "b")){
      message("Some rows contain information for both eyes. Correct?")
    }
    x[[eye]] <- recode_eye$result
    res <- count_eyes(x = x, pat_col = pat_col, eye = eye)
  }
  res
}

#' internal count
#' @name count_eyes
#' @param x object (data frame)
#' @param pat_col patient column
#' @param eye eye column
#' @description `count_eyes()` is the internal counting function
#' @return Named integer vector with count of patients and eyes
#' @keywords internal
#'   for [`eyes()`]
count_eyes <- function(x, pat_col, eye) {
  n_pat <- length(unique(x[[pat_col]]))

  rl <- c("r", "l")
  x[[eye]] <- factor(x[[eye]], levels = union(c("b", rl), unique(x[[eye]])))
  eye_tab <- table(unique(x[, c(pat_col, eye)]))

  if (any(grepl("b", colnames(eye_tab)))) {
    if (any(grepl(paste(rl, collapse = "|"), colnames(eye_tab)))) {
      eye_tab[, rl][eye_tab[, "b"] == 1] <- 0
    }
  }

  out <- colSums(eye_tab)
  outfinal <- out[rl] + out["b"]
  n_eyes <- sum(outfinal)
  nr <- unname(outfinal["r"])
  nl <- unname(outfinal["l"])

  return(c(patients = n_pat, eyes = n_eyes, right = nr, left = nl))
}

#' @rdname eyes
#' @inheritParams eyes
#' @param ... passed to [eyes]
#' @inheritParams eyes_to_string
#' @return `eyestr`: Character string - can be directly pasted into reports
#' @examples
#' library(eyedata)
#' eyestr(amd2, para = TRUE)
#' @export
eyestr <- function(x, ..., small_num = TRUE, para = FALSE, UK = FALSE){
  res <- suppressMessages(eyes(x, ...))
    res_str <- res[c("patients", "eyes")]
    eyes_to_string(res_str[!is.na(res_str)],
                   small_num = small_num, para = para, UK = UK)
  }

#' Eye count to strings
#' @name eyes_to_string
#' @param x vector of one or two
#' @param small_num If TRUE: writing numbers <= 12 as words
#' @param para If TRUE: Adding "A total of" to comply with most
#' journal standards and to avoid awkward long numbers.
#' @param UK Logical, Use UK (English) style (TRUE) or
#'   USA (American) style (FALSE).
#' @return Character string - can be directly pasted into reports
#' @keywords internal
#' @importFrom english english

eyes_to_string <- function(x, small_num = TRUE, para = FALSE, UK = FALSE) {
  if (para) {
    para <- "A total of "
  } else {
    para <- NULL
  }
  if (isTRUE(small_num)) {
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

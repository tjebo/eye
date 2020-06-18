#' Count patients and eyes
#' @name eyes
#' @description Counts number of patients and eyes (right and left).
#' @param x required. (data frame)
#' @param id Patient identifying column
#' @param eye Eye identifying column.
#' @param report if TRUE, text returned for report
#' @param ... passed to [eyes_to_string]
#' @inheritParams eyes_to_string
#' @details
#' `eyes` guesses columns that identify patients and eyes.
#' @section Guessing:
#' For any below, **cases are always ignored** (you can write in
#' upper or lower case, as you please)
#'
#' **id** and **eye** arguments overrule the name guessing for
#' the respective columns.
#'
#' **patient ID columns**:
#' - First, `eyes` is looking for names that contain both strings
#' "pat" and "id" (the order doesn't matter)
#' - Next, it will look for columns that are plainly called "ID"
#' - Last, it will search for all names that contain either "pat"
#' or "id"
#'
#' **eye variable column**:
#' - `eyes` looks for columns called either "eye" or "eyes"
#'
#' @section Eye coding:
#' - `eyes` recognizes integer coding 0:1 and 1:2, with right being
#'    the lower number. For strings coding it recognizes
#'    right eyes: c("r", "re", "od", "right") and
#'    left eyes: c("l", "le", "os", "left")
#' @section Report:
#'  Using [eyes_to_string] to parse the output of `eyes` into
#'  a text which you can use for reports. Arguments to `eyes_to_string`
#'  are passed via **...**:
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
#' eyes(amd)
#' @export
#'
eyes <- function(x, id = NULL, eye = NULL, report = FALSE, ...) {
  if (is.null(id)) {
    pat_col <- getElem_id(x)
  } else {
    pat_col <- id
  }
  if (length(pat_col) != 1) {
    warning("Did not find the ID column - use argument \"id\"",
            call. = FALSE)
    return(NULL)
  }
  if (is.null(eye)) {
    eye <- whole_str(c("eyes", "eye"))(colnames(x))
  }
  if (length(eye) > 1) {
    warning("Which is the eye column? Use argument \"eye\"", call. = FALSE)
    return(NULL)
  }
  if (length(eye) < 1) {
    message("No eye column found: Counting patients only")
    res <- c(patients = length(unique(x[[pat_col]])))
  } else if (length(eye) == 1) {
    quiet_recode <- purrr::quietly(recodeye)
    recode_eye <- quiet_recode(x[[eye]])
    if(length(recode_eye$warnings)>0){
      warning(paste("Unclear eye coding! Not recoded. Please clean data.
            Your values:", paste(unique(x[[eye]]), collapse = ", ")), call. = FALSE)
      return(NULL)
    }
    if(length(recode_eye$messages)>0){
        message(gsub("\\\n", "", recode_eye$messages))
    }
    x[[eye]] <- recode_eye$result

    if (sum(is.na(x[[eye]]) > 0)) {
      message("Not all eyes are identified (contains NA)")
    }
    res <- count_eyes(x = x, pat_col = pat_col, eye = eye)
  }
  if (report) {
    res_str <- res[c("patients", "eyes")]
    eyes_to_string(res_str[!is.na(res_str)], ...)
  } else {
    res
  }
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
  eye_tab <- table(unique(x[, c(pat_col, eye)]))
  n_r <- unname(colSums(eye_tab[, colnames(eye_tab) == "r", drop = FALSE]))
  n_l <- unname(colSums(eye_tab[, colnames(eye_tab) == "l", drop = FALSE]))
  n_eyes <- n_r + n_l
  res <- c(patients = n_pat, eyes = n_eyes, right = n_r, left = n_l)
  res
}

#' Eye count to string
#' @rdname eyes
#' @description `eyestr`: identical to `eyes(x, report = TRUE, ...)`
#' @return `eyestr`: Character string - can be directly pasted into reports
#' @examples
#' eyestr(amd, para = TRUE)
#' @export
eyestr <- function(x, id = NULL, eye = NULL, small_num = TRUE, para = FALSE, UK = FALSE){
suppressMessages(eyes(x = x, id = id, report = TRUE, eye = eye, small_num = small_num, para = para, UK = UK))
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

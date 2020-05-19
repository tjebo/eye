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

#' counteyes
#' @rdname eyes
#' @export
counteyes <- eyes

#' count_eyes
#' @rdname eyes
#' @export
count_eyes <- eyes

#' long eye data
#' @description Wrapper around [tidyr::pivot_longer()] to make your data
#' long ("myopic")
#' @name myop
#' @param x data frame
#' @param cols Eye columns either automatic or with vector of two - see details
#' @param eye_code Vector of two with right eye coded first.
#' @param values_to passed to [tidyr::pivot_longer()]
#' @param names_to passed to [tidyr::pivot_longer()]
#' @param ... other arguments passed to [tidyr::pivot_longer()]
#' @details
#' - **cols**: if not specified, `myop()` will look for columns that have
#' typical eye codes, i.e. c("r", "re" and "od") for right eyes and
#' c("l", "le" and "os") for left eyes.
#' - if argument specified, right eye has to be given first!
#' - **eye_code**: numeric codes of 0:1 or 1:2 are allowed, other
#' numeric codes are not supported.
#' @eye_code
#' @import tidyr
#' @examples
#' set.seed(42)
#' iop <- data.frame(id = letters[1:11], r = sample(10:20), l = sample(10:20))
#'
#' myop(iop, values_to = "iop")
#'
#' # Example to clean a bit messy data frame
#' iop_va <- data.frame(id = letters[1:11],
#'                      iop_r = sample(10:20), iop_l = sample(10:20),
#'                      va_r = sample(40:50), va_l = sample(40:50))
#' # use myope twice on both iop and va columns
#' iop_long <- myop(iop_va, cols = c("iop_r", "iop_l"), values_to = "iop")
#' va_long <- myop(iop_va, cols = c("va_r", "va_l"), values_to = "va")
#' # full join both data frames
#' iop_va_clean <- full_join(iop_long, va_long, by = c("id", "eye")) %>%
#'   select(id, eye, va, iop)
#'
#' @export

myop <- function(x, cols, values_to, names_to = "eye", eye_code = c("r", "l"), ...) {
  eye_r <- c("r", "re", "od")
  if (missing(values_to)) {
    values_to <- "value"
  }
  if (length(values_to) > 1) {
    stop("Only one value should be provided")
  }
  eye_l <- c("l", "le", "os")
  if (eye_code[1] %in% eye_l | eye_code[2] %in% eye_r) {
    stop("right eyes need to be coded first", call. = FALSE)
  }
  if (identical(eye_code, 0:1) | identical(eye_code, 1:2)) {
    message("Consider characters instead of numeric eye coding. Converted to characters")
    eye_code <- as.character(eye_code)
  } else if (!eye_code[1] %in% eye_r | !eye_code[2] %in% eye_l) {
    if (sum(!is.na(as.integer(eye_code))) > 0) {
      stop("Numeric coding apart from 0:1 or 1:2 not supported", .call = FALSE)
    }
    warning("Very unusual way of coding for eyes", call. = FALSE)
  }
  if (missing(cols)) {
    low_col <- tolower(names(x))
    col_r <- low_col[low_col %in% eye_r]
    col_l <- low_col[low_col %in% eye_l]
    if (length(col_r) != 1 | length(col_l) != 1) {
      stop("Eye columns ambiguous. Fix with cols argument or change variables names", call. = FALSE)
    }
    names(x)[names(x) %in% c(col_r, col_l)] <- eye_code
    cols <- rlang::expr(c(eye_code[1], eye_code[2]))
  } else {
    if(length(cols)!=2){
      stop("Two variables have to be specified.", call. = FALSE)
    }
    names(x)[names(x) %in% c(cols[1], cols[2])] <- eye_code
    cols <- rlang::expr(c(eye_code[1], eye_code[2]))
  }
  tidyr::pivot_longer(x, cols = !!cols, names_to = names_to, values_to = values_to, ...)

}

#' myope
#' @rdname myop
#' @export
myope <- myop

#' myopise
#' @rdname myop
#' @export
myopise <- myop

#' myopize
#' @rdname myop
#' @export
myopize <- myop

#' myopic
#' @rdname myop
#' @export
myopic <- myop

#' Visual acuity conversion
#' @description Cleans VA and converts VA notations (classes) into one another.
#'   * `va` will guess the VA class based on specific rules (see details)
#'   * `va_dissect`: wrapper around [va_dissect_apply], converting each element
#'   individually
#' @param x Vector with visual acuity entries. Must be atomic.
#' @param from from which class to convert. "etdrs", "logmar" or "snellen" -
#' any case allowed. Ignored if it does not make sense
#' @param to to which class to convert. "etdrs", "logmar" or "snellen" -
#' any case allowed
#' @param snellen_type if to = "snellen", which snellen notation.
#'   "m", "dec" or "ft"
#' @details
#'   **`va`** cleans visual acuity entries with [clean_va]: it assigns `NA`
#'   to missing entries, removes "plus" and "minus" from snellen entries
#'   and unifies the notation for qualitative entries.
#'
#'   **`va_dissect`** is very similar to `va`. However, it is less flexible
#'   and will always convert to logmar. It also does not support
#'   snellen decimals!
#'
#'   Distinction between logMAR and snellen decimals, or between ETDRS and
#'   logMAR can be ambiguous in some special cases and **logmar** is picked -
#'   provide *from* if you want something else.
#'
#'   **snellen decimals** are a particular challenge and `va` may wrongly
#'   assign  logMAR - this could happen if you have weird snellen decimal
#'   values that are not found in [va_chart]. E.g., check with `unique(x)`.
#'
#'   *quali* class represents "(No) light perception", "hand movements"
#'   and "counting fingers" and are converted to logmar following the
#'   suggestions by [Michael Bach](https://michaelbach.de/sci/acuity.html)
#'
#'   **Conversion rules**:
#'   - if x integer and 3 < x <= 100: `etdrs`
#'   - if x numeric and -0.3 <= x <= 3: `logmar`
#'   - *va only* if `all(x %in% va_chart$snellen_dec)`: `snellen`
#'   - if x those ranges: `NA`
#'   - if character and format x/y: `snellen` (fraction)
#'   - if one of "CF", "HM", "LP", "PL", "NLP", or "NPL": `quali`
#'   - *va_dissect only*: No snellen decimal supported!
#'   - *va_dissect only*: if NA: quali (but that's rather irrelevant)
#'   - Any other string: `NA`
#' @return
#' - **va**: vector of class set with `to` argument
#' - **va_dissect**: Named vector of class `logmar`
#' @family Ophthalmic functions
#' @family VA converter
#' @family VA cleaner
#' @examples
#' ## will automatically detect VA class and convert to logMAR by default
#' ## ETDRS letters
#' va(c(23, 56, 74, 58))
#'
#' ## ... or convert to snellen
#' va(c(23, 56, 74, 58), to = "snellen")
#'
#' ## snellen, mixed with categories. Also dealing with those "plus/minus" entries
#' va(c("NLP", "NPL", "PL", "LP", "HM", "CF", "6/60", "20/200", "6/9", "20/40", "20/40+3", "20/50-2"))
#'
#' ## on the inbuilt data set:
#' head(va(amd$VA_ETDRS_Letters), 10)
#'
#' @name va
#' @importFrom rlang sym
#' @export
va <- function(x, from = NULL, to = "logmar", snellen_type = "ft") {
  if(!is.atomic(x)){
    stop("x must be atomic", call. = FALSE)
  }
  x_sym <- deparse(substitute(x))
  to <- tolower(to)
  if (length(to) != 1 | !to %in% c("etdrs", "logmar", "snellen")) {
    stop("\"to\": Pick one of \"etdrs\", \"logmar\" or \"snellen\"", call. = FALSE)
  }
  if (inherits(x, "va")) {
    set_va <- class(x)[class(x) %in% c("logmar", "snellen", "etdrs", "quali")]

    if (!is.null(from)) {
      warning("Ignoring \"from\": overriden by VA class", call. = FALSE)
    }
    if (to == set_va) {
      message("VA already in desired class. No conversion made")
      return(x)
    } else {
      class_va <- set_va
    }
  } else {
    if (is.character(x) | is.factor(x)) {
      x <- clean_va(x)
    }
    guess_va <- which_va(x)
    if(any(guess_va %in% "mixed")){
    stop("Mixed object - not supported. Try with va_dissect", call. = FALSE)
    }
    if (length(guess_va) == 2) {
      if(any(guess_va %in% "implaus")){
        warning("Implausible values replaced with NA! Check your data",
              call. = FALSE
      )
        class_va <- guess_va[1]
      } else if (is.null(from)) {
        warning(paste("Wavering between", guess_va[1], "and", guess_va[2], "- picked logMAR!
              If unhappy, change with \"from\""), call. = FALSE)
        class_va <- "logmar"
      } else if (length(from) > 1 | !tolower(from) %in% guess_va) {
        warning(paste("Ignoring \"from\": Pick", paste(guess_va, collapse = " or ")), call. = FALSE)
        class_va <- "logmar"
      } else {
        class_va <- guess_va[guess_va == tolower(from)]
      }
    } else {
      if (guess_va == "failed") {
        stop(paste("Failed to detect VA class. Clean your data -
           your unique values:", paste(unique(x), collapse = ", ")), call. = FALSE)
      }
      class_va <- guess_va
      if (!is.null(from)) {
        if (tolower(from) != class_va) {
          message(paste("From", guess_va))
          warning(paste("Ignoring \"from\":", from, "implausible"), call. = FALSE)
        }
      } else {
        message(paste("From", guess_va))
      }
    }

    if (class_va == to) {
      message(paste(
        "VA already in the desired notation. Updated", x_sym, "to", class_va
      ))
    }
  }
  class(x) <- class_va

  if (to == "snellen") {
    if (!snellen_type %in% c("ft", "m", "dec")) {
      warning("Ignoring snellen_type (pick one of ft, m, or dec). Converting to ft",
        call. = FALSE
      )
      snellen_type <- "ft"
    }
    eval(rlang::sym(paste0("va_", to)))(x, snellen = snellen_type)
  } else {
    eval(rlang::sym(paste0("va_", to)))(x)
  }
}

#' @rdname va
#' @export
convert_va <- va

#' @rdname va
#' @export
va_dissect <- function(x) {
  if(!is.atomic(x)){
    stop("x must be atomic", call. = FALSE)
  }
  x_clean <- clean_va(as.character(x))
  new_va <- sapply(x_clean, va_dissect_apply, USE.NAMES = FALSE)
  new_va
}

#' VA helper
#' @name va_helper
#' @param x Vector with VA entries
#' @description Internal functions which help with VA conversion
#' * `which_va`: guessing which VA notation (VA class)
#' @family VA helper
#' @family VA converter
#'
which_va <- function(x) {
  x <- suppressWarnings(as.character(x))

  if (all(x[!is.na(x)] %in% set_quali())) {
    return("quali")
  }
  x_noquali <- x[!x %in% set_quali()]
  x_num <- suppressWarnings(as.numeric(x_noquali))
  if (all(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("snellen")
  } else if (any(grepl("/", x_noquali[!is.na(x_noquali)]))) {
    return("mixed")
  }

  if (all(is.na(x_num))) {
    return("failed")
  }
  x_numval <- x_num[!is.na(x_num)]
  if (length(x_numval) < length(x_noquali[!is.na(x_noquali)])) {
    warning("Introduced NAs - Mixed with snellen or other categories?", call. = FALSE)
  }

  if (all(x_numval == as.integer(x_numval))) {
    if (all(x_numval %in% 0:3)) {
      return(c("logmar", "etdrs"))
    } else if (all(x_numval >= 0) & all(x_numval <= 100)) {
      return("etdrs")
    } else {
      return(c("etdrs", "implaus"))
    }
  }

  if (all(round(x_num[!is.na(x_num)], 2) %in% inter_snelllog)) {
    return(c("logmar", "snellen"))
  } else if (all(round(x_num, 3) %in% as.numeric(va_chart$snellen_dec))) {
    return("snellen")
  } else if (any(x_numval < -0.3 | any(x_numval > 3))) {
    return(c("logmar", "implaus"))
  } else {
    return("logmar")
  }
}

#' @rdname va_helper
#' @description * `va_dissect_apply`: converts every element individually
va_dissect_apply <- function(elem) {
  guess_va <- which_va(elem)

  if (length(guess_va) == 2) {
    if (any(guess_va %in% "implaus")) {
      warning("NA introduced. Implausible values", call. = FALSE)
    }
    guess_va <- "logmar"
    class_va <- "logmar"
  } else if (guess_va == "failed") {
    warning("NA introduced. Snellen? Needs to be in format x/y for conversion",
      call. = FALSE
    )
    elem <- NA
    class_va <- "logmar"
  } else {
    class_va <- guess_va
  }

  class(elem) <- class_va

  # return(class(elem))
  elem_logmar <- va_logmar(elem)
  names(elem_logmar) <- guess_va

  return(elem_logmar)
}

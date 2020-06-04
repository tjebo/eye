#' Visual acuity notation conversion
#' @description Cleans VA and converts VA notations (classes) into one another.
#'   `va` will guess the VA class based on specific rules (see details)
#' @param x Vector with visual acuity entries. Must be atomic.
#' @param from from which class to convert. "etdrs", "logmar" or "snellen" -
#' any case allowed. Ignored if it does not make sense
#' @param to to which class to convert. "etdrs", "logmar" or "snellen" -
#' any case allowed
#' @param type if to = "snellen", which snellen notation.
#'   "m", "dec" or "ft"
#' @name va
#' @details
#'   VA can be snellen feet/meter/decimal, logMAR, ETDRS, or
#' "qualitative" (Couting fingers, etc.), and a mix of those!
#'
#'   - Snellen fractions need to be either form 6/x or 20/x
#'   - ETDRS must be between 0 and 100
#'   - logMAR must be between -0.3 and 3.0
#'   - Qualitative must be PL, LP, NLP, NPL, HM, CF (any case allowed)
#'   For further conversion rules see section "VA guessing"
#'
#'   **`va`** cleans visual acuity entries with [clean_va]: it assigns `NA`
#'   to missing entries, removes "plus" and "minus" from snellen entries
#'   and unifies the notation for qualitative entries.
#'
#'   **`va_dissect`** is very similar to `va`. However, it is less flexible
#'   and will always convert to logmar. It also does not support
#'   snellen decimals! Usually you don't need to use it -
#'   It is internally called when [which_va] guesses "mixed" notation.
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
#' @section VA guessing:
#'   - if x integer and 3 < x <= 100: `etdrs`
#'   - if x integer and 0 <= x <= 3: `logmar`, but you can choose `etdrs`
#'   - if x numeric and -0.3 <= x <= 3: `logmar`
#'   - if x numeric and all x in intersection(va_chart$logMAR, va_chart$snellen_dec):
#'     `logmar`, but you can choose `snellen`
#'   - *non-mixed class*: if all x in va_chart$snellen_dec: `snellen`
#'   - *mixed class* ([which_va_dissect]): snellen_dec not supported.
#'   - if character and format x/y: `snellen` (fraction)
#'   - if one of "CF", "HM", "LP", "PL", "NLP", or "NPL": `quali`
#'   - if numeric x beyond the ranges from above: `NA`
#'   - Any other string or NA: `NA`
#' @return
#' - **va**: vector of class set with `to` argument
#' - **va_dissect**: Named vector of class `logmar`
#' @family Ophthalmic functions
#' @family VA converter
#' @family VA cleaner
#' @examples
#' ## will automatically detect VA class and convert to logMAR by default
#' ## ETDRS letters
#' x <- c(23, 56, 74, 58)
#' va(x)
#'
#' ## ... or convert to snellen
#' va(x, to = "snellen")
#'
#' ## snellen, mixed with categories. Also dealing with those "plus/minus" entries
#' va(c("NLP", "NPL", "PL", "LP", "HM", "CF", "6/60", "20/200", "6/9",
#'  "20/40", "20/40+3", "20/50-2"))
#'
#' @importFrom rlang sym
#' @export
va <- function(x, from = NULL, to = "logmar", type = "ft") {
  if (!is.atomic(x)) {
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
      warning("Ignoring \"from\": overruled by VA class", call. = FALSE)
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
    if (all(guess_va == "NA")) {
      warning(paste0(
        "No conversion (",
        x_sym,
        ") - vector of NA"
      ), call. = FALSE)
      return(x)
    }
    if (any(guess_va %in% "mixed")) {
      message(paste0(
        "Mixed object (",
        x_sym,
        ") - using va_dissect()"
      ))
      new_va <- sapply(as.character(x), which_va_dissect, USE.NAMES = FALSE)
      return(new_va)
    }
    if (length(guess_va) == 2) {
      if (any(guess_va %in% "implaus")) {
        warning(paste0(
          "NA introduced (",
          x_sym,
          ") - implausible values"
        ),
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
        warning(paste0(
          "No conversion (", x_sym, ") - characters only:\n",
          paste(unique(x), collapse = ", ")
        ),
        call. = FALSE
        )
        return(x)
      }
      class_va <- guess_va
      if (!is.null(from)) {
        if (tolower(from) != class_va) {
          message(paste0(x_sym, ": from ", guess_va))
          warning(paste("Ignoring \"from\":", from, "implausible"), call. = FALSE)
        }
      } else {
        message(paste0(x_sym, ": from ", guess_va))
      }
    }

    if (class_va == to) {
      message(paste(
        x_sym, "already in desired notation. Updated to class", class_va
      ))
    }
  }
  class(x) <- class_va

  if (to == "snellen") {
    if (!type %in% c("ft", "m", "dec")) {
      warning(
      paste0("Ignoring \"type = ",
             type, "\" - must be ft, m, or dec. Converting to ft"),
        call. = FALSE
      )
      type <- "ft"
    }
    }
convertVA(x, to = to, snellnot = type)
}

#' @rdname va
#' @export
convert_va <- va

#' @rdname va
#' @export
va_dissect <- function(x) {
  if (!is.atomic(x)) {
    stop("x must be atomic", call. = FALSE)
  }
  x_clean <- clean_va(as.character(x))
  new_va <- sapply(x_clean,
    function(elem) {
      class_va <- which_va_dissect(elem)
      class(elem) <- class_va
      convertVA(elem, to = "logmar")
      }, USE.NAMES = FALSE)

  new_va
}

#' Guessing the VA class
#' @name which_va
#' @param x Vector with VA entries
#' @description Guessing the VA notation (VA class)
#' * `which_va`: guessing VA class for entire vector
#' @family VA helper
#' @family VA converter
#'
which_va <- function(x) {
  x <- tolower(suppressWarnings(as.character(x)))
  if (all(is.na(x))) {
    return("NA")
  }
  if (all(x[!is.na(x)] %in% unlist(set_codes()["quali"]))) {
    return("quali")
  }
  x_noquali <- x[!x %in% unlist(set_codes()["quali"])]
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
    return("mixed")
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

#' @rdname which_va
#' @param elem element of vector to convert
#' @description * `which_va_dissect`: guessing VA class for each vector element
which_va_dissect <- function(elem) {
  guess_va <- which_va(elem)

  if (length(guess_va) == 2) {
    if (any(guess_va %in% "implaus")) {
      warning("NA introduced (implausible values)", call. = FALSE)
    }
    guess_va <- "logmar"
    class_va <- "logmar"
  } else if (guess_va == "failed") {
    warning("NA introduced (character only). See ?va for help",
      call. = FALSE
    )
    elem <- NA
    class_va <- "logmar"
  } else {
    class_va <- guess_va
  }
  class_va
}

#' va
#' @name va
#' @description VA notation conversion
#' @param x vector with VA values
#' @param from String, from which notation to convert (See details)
#' @param to String, to which notation to convert. Default logMAR (See details)
#' @param snellen_type which snellen method to use when *to = "snellen"*.
#'   Either "m", "dec" or "ft"
#' @family ophthalmic functions
#' @details **from** if not provided (default), `va` will try to guess.
#'   This will work in most cases, but can fail due to weird, unpredicted ways
#'   to enter and save data. For more control, provide string:
#'   Either "ETDRS", "logMAR" or "snellen" (any case allowed).
#'   **to** Supported: "ETDRS", "logMAR" or "snellen" (any case allowed).
#' @examples
#' TBC
#' @details
#' Qualitative vision measures "Light perception", "hand movements" and "counting fingers"
#' have to be coded "LP", "HM" and "CF", otherwise they will be replaced with NA.
#' "No light perception" is not converted into visual acuity values and will be replaced with NA.
#' @importFrom rlang sym
#'
#' @export
va <- function(x, from = NULL, to = "logmar", snellen_type = "ft") {
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

    if (length(guess_va) == 2) {
      if (is.null(from)) {
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
          warning(paste("Ignoring \"from\":", from, "not plausible"), call. = FALSE)
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

#' VA conversion
#' @name va_methods
#' @description S3 methods for VA conversion
#' @param x vector of visual acuities
#' @family va conversion methods
va_logmar <- function (x, ...) {
  UseMethod("tologMAR", x)
}

#' va_etdrs
#' @rdname va_methods
#' @param x vector of visual acuities
#' @family va conversion methods
va_etdrs <- function (x, ...) {
  UseMethod("toETDRS", x)
}

#' va_snellen
#' @rdname va_methods
#' @param x vector of visual acuities
#' @param snellen which Snellen notation to display. Either "sn", "m" or "dec"
#' @family va conversion methods
va_snellen <- function (x, snellen = "ft") {
  UseMethod("toSnellen", x)
}

#' which_va
#' @rdname internals
#' @param x vector
#' @description internal check which type of va in order to assign class.
#' @family va conversion functions
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
  } else if (any(grepl("/", x_noquali[!is.na(x_noquali)]))){
    stop("Mixed object - currently not supported", call. = FALSE)
  }


  if (all(is.na(x_num))) {
    warning("Snellen? Needs to be in format \"x/y\" for conversion",
      call. = FALSE
    )
    return("failed")
  }
  x_numval <- x_num[!is.na(x_num)]
  if (length(x_numval) < length(x_noquali[!is.na(x_noquali)])) {
    warning("Introduced NAs - Mixed with snellen or other categories?", call. = FALSE)
  }

  if (all(x_numval == as.integer(x_numval))) {
    if (all(x_numval %in% 0:3)){
      return(c("logmar", "etdrs"))
      } else if (all(x_numval >= 0) & all(x_numval <= 100)) {
      return("etdrs")
    } else {
      warning("Unplausible values! Check your data",
        call. = FALSE
      )
      return("etdrs")
    }
  }

  if (all(round(x_num[!is.na(x_num)], 2) %in% inter_snelllog)) {
    return(c("logmar", "snellen"))
  } else if (all(round(x_num, 3) %in% as.numeric(va_chart$snellen_dec))) {
    return("snellen")
  } else if (any(x_numval < -0.3 | any(x_numval > 3))) {
    warning("Unplausible values! Check your data",
      call. = FALSE
    )
    return("logmar")
  } else {
    return("logmar")
  }
}

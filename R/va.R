#' Visual acuity notation conversion
#' @description Cleans and converts visual acuity notations (classes)
#' between Snellen (decimal, meter and feet), ETDRS, and logMAR.
#'   `va` detects the VA class and will convert to logMAR as default.
#' @param x Vector with visual acuity entries. Must be atomic.
#' Snellen fractions need to be entered with "/"
#' @param to To which class to convert. "etdrs", "logmar" or "snellen" -
#' any case allowed
#' @param type To which Snellen notation to convert: "m", "dec" or "ft"
#' @param from_logmar chose logmar when guessing between two notations (logmar
#'   vs. snellen decimal or logmar vs. etdrs)
#' @param logmarstep how +/- entries are evaluated. FALSE:
#'   increase/decrease Snellen fractions by lines. TRUE: plus/minus
#'   entries equivalent to 0.02 logmar or 1 ETDRS letter
#' @param mixed TRUE Elements will be converted one by one.
#'   Most plausibility checks will be overruled!
#' @name va
#' @details Each class can be converted from one to another, and va()
#' converts to logMAR by default. In case of ambiguous detection,
#' logMAR is selected as default, or the other alternative is selected with
#' `from_logmar = FALSE`.
#' @section VA conversion:
#' - **logMAR to ETDRS**: logMAR rounded to the first digit and converted with
#' the chart.
#' - **Snellen to logMAR**: logMAR = -1 * log10(snellen_frac)
#' - **Snellen to ETDRS**: ETDRS = 85 + 50 * log10(snellen_frac)
#' [Gregori et al.](https://doi.org/10.1097/iae.0b013e3181d87e04).
#' - **ETDRS to logMAR**: logMAR = -0.02 * etdrs + 1.7
#' [Beck et al.](https://doi.org/10.1016/s0002-9394(02)01825-1)
#' - **Hand movements and counting fingers** are converted following
#' [Schulze-Bonsel et al.](https://doi.org/10.1167/iovs.05-0981)
#' - **(No) light perception** are converted following the suggestions by
#' [Michael Bach](https://michaelbach.de/sci/acuity.html)
#' - **To Snellen**:
#' Although there seems to be no good statistical reason to convert
#' back to Snellen, it is a very natural thing to eye specialists to think
#' in Snellen. A conversion to snellen gives a good gauge of how the visual
#' acuity for the patients are. However, back-conversion should not be
#' considered an exact science and any attempt to use formulas will result
#' in very weird Snellen values that have no correspondence to common charts.
#' Therefore, Snellen matching the nearest ETDRS and logMAR value in
#' the [va_chart] are used.
#'
#' @section Accepted VA formats:
#' - Snellen fractions (meter/ feet) need to be entered as fraction with
#' "/".
#' - **when converting to ETDRS or logMAR**: any fraction is allowed ,
#' e.g. 3/60 and 2/200 will also be recognized.
#' - **When converting between Snellen fractions**:
#' *has to be either 6/ or 20/*. Other fractions will not be recognized -
#' see **"Examples"**
#' - ETDRS must be integer-equivalent between 0 and 100 (integer equivalent
#' means, it can also be a character vector)
#' - logMAR must be between -0.3 and 3.0
#' - Qualitative must be either of PL, LP, NLP, NPL, HM, CF (any case allowed)
#' - Any element which is not recognized will be converted to NA
#' - Vectors containing several notations ("mixed") are guessed and converted
#'   element by element with [which_va_dissect] and [va_dissect]
#'
#' @section VA detection:
#'   - Internally done with [which_va()] based on the following rules
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
#'   - Any other string or NA: NA
#'
#' Detection and conversion is on a vector as a whole by [which_va()]. If a
#' "mixed" VA notation
#' is found, [which_va_dissect()] and [va_dissect()] will be called instead
#' for each VA vector element individually.
#'
#' @section Problematic cases:
#' There can be ambiguous cases for detection (detection defaults to logmar):
#' x is one of 0,1,2,3 - This can be ETDRS and logMAR.
#' x is one of c(1.5, 1, 0.8, 0.5, 0.4, 0.3, 0.2, 0.1, 0) - This can be
#' snellen decimal or logMAR.
#'
#' **snellen decimals** are a particular challenge and `va` may wrongly
#'   assign  logMAR - this could happen if there are unusual snellen decimal
#'   values in the data which are not part of [va_chart]. E.g., check the
#'   values with `unique(x)`.
#' @section Snellen "+/-" entries:
#' By default, plus/minus entries are evaluated as intended by the
#' test design: Snellen fractions increase/decrease only by lines.
#'
#'     - if entry -1 to +3 : take same Snellen value
#'     - if <= -2 : take Snellen value one line below
#'     - if >+3 (unlikely, but unfortunately not impossible):
#'
#' If logmarstep = TRUE, each snellen optotype will be considered
#' equivalent to 0.02 logmar or 1 ETDRS letter (assuming 5 letters
#' in a row in a chart)
#' @section VA cleaning:
#' For more details see [clean_va()]
#' 1. `NA` is assigned to strings such as "." or "", "n/a" or "   "
#' 1. notation for qualitative entries is simplified.
#' @section VA classes:
#' convert_VA returns a vector of three classes:
#'   1. `va`
#'   1. One of `snellen`, `logmar`, `etdrs` or `quali`.
#'   1. Either of `character` (for Snellen and qualitative),
#'       `numeric` (for logMAR), or `integer` (for ETDRS).
#' @return vector of `va` class. See also "VA classes"
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
#' ## A mix of notations is also possible
#' x <- c("NLP", "0.8", "34", "3/60", "2/200", "20/40+3", "20/50-2")
#' va(x)
#'
#' ## Any fraction is possible, and empty values
#' x <- c("CF", "3/60", "2/200", "", "20/40+3", ".", "      ")
#' va(x)
#'
#' ## but this not any fraction when converting from one class to the other
#' x <- c("3/60", "2/200", "6/60", "20/200", "6/9")
#' va(x, to="snellen", type = "m")
#' @export
va <- function(x, to = "logmar",
               type = NULL,
               from_logmar = TRUE,
               logmarstep = FALSE,
               mixed = FALSE) {
  if (!is.atomic(x)) {
    stop("x must be atomic", call. = FALSE)
  }
  x_sym <- deparse(substitute(x))
  to <- tolower(to)
  if (length(to) != 1 | !to %in% c("etdrs", "logmar", "snellen")) {
    stop("\"to\": Pick one of \"etdrs\", \"logmar\" or \"snellen\"", call. = FALSE)
  }
  if (to == "snellen") {
    if (is.null(type)) {
      type <- "ft"
    } else if (!type %in% c("ft", "m", "dec")) {
      warning(
        paste0(
          "Ignoring \"type = ",
          type, "\" - must be ft, m, or dec. Converting to ft"
        ),
        call. = FALSE
      )
      type <- "ft"
    }
  }
  if (inherits(x, "va")) {
    set_va <- class(x)[class(x) %in% c("logmar", "snellen", "etdrs", "quali")]

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
      warning(paste0("No conversion (", x_sym, ") - vector of NA"), call. = FALSE)
      return(x)
    }
    if (any(guess_va %in% "mixed")| isTRUE(mixed)) {
      message(paste0("Mixed object (", x_sym, ") - converting one by one"))
      new_va <- va_dissect(x,
        to = to,
        snellnot = type,
        logmarstep = logmarstep,
        from_logmar = from_logmar
      )
      return(new_va)
    }
    if (length(guess_va) == 2) {
      if (any(guess_va %in% "implaus")) {
        warning(paste0(
          x_sym, " (from ", guess_va[!guess_va %in% "implaus"],
          "): NA introduced - implausible values"
        ),
        call. = FALSE
        )
        class_va <- guess_va[!guess_va %in% "implaus"]
      } else if (isTRUE(from_logmar)) {
        message(
          paste(
            "Notation ambiguous - logMAR picked. Change to",
            guess_va[!guess_va %in% "logmar"] ,
            "with \"from_logmar = FALSE\""
          )
        )
        class_va <- "logmar"
      } else {
        message(paste0(x_sym, ": from ", guess_va[!guess_va %in% "logmar"]))
        class_va <- guess_va[!guess_va %in% "logmar"]
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
      message(paste0(x_sym, ": from ", guess_va))
    }

  }
  class(x) <- class_va

  convertVA(x, to = to, snellnot = type, logmarstep = logmarstep)
}

#' Converting each VA element
#' @param x vector of VA.
#' @param to to which VA notation
#' @param ... passed to convertVA
#' @description Used in [va()] if  [which_va()] finds "mixed" VA notation.
#' Converts each VA vector element individually - requires the VA vector to
#' be prepared with [clean_va()] first.
#' @rdname va_dissect
#' @family VA converter
#' @return vector of `va` class. see also [va()]
#' @keywords internal
va_dissect <- function(x, from_logmar, to, ...) {
  new_va <- sapply(x,
    function(elem) {
      class_va <- which_va_dissect(elem, from_logmar = from_logmar)
      class(elem) <- class_va
      convertVA(elem, to = to, ...)
      }, USE.NAMES = FALSE)
  class(new_va) <- c(to, "va", class(new_va))
  new_va
}

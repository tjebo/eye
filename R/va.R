#' Visual acuity notation conversion
#' @description Cleans and converts visual acuity notations (classes)
#'   between Snellen (decimal, meter and feet), ETDRS, and logMAR.
#'   `va` detects the VA class and will convert to logMAR as default.
#' @param x Vector with visual acuity entries. Must be atomic.
#' Snellen fractions need to be entered with "/"
#' @param from will force to evaluate from which notation to convert -
#'   only really required if weird snellen decimal entries
#'   or in rare cases with only few low ETDRS (0 to 3). Must be either
#'   "etdrs", "logmar" or "snellen"
#' @param to To which class to convert. "etdrs", "logmar" or "snellen" -
#'   any case allowed. If NULL (default), will simply "clean up" VA entries.
#'   This may then result in a vector of "mixed" VA notations.
#' @param type To which Snellen notation to convert: "m", "dec" or "ft"
#' @param smallstep how +/- entries are evaluated. FALSE:
#'   increase/decrease Snellen fractions by lines. TRUE: plus/minus
#'   entries equivalent to 0.02 logmar or 1 ETDRS letter
#' @param noplus ignoring plus/minus entries and just returning the
#'     snellen fraction. This overrides the smallstep argument.
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
#'
#' @section VA detection:
#'   - Internally done with [which_va()] based on the following rules
#'   - if x integer and 3 < x <= 100: `etdrs`
#'   - if x integer and 0 <= x <= 3: `logmar`, but you can choose `etdrs`
#'   - if x numeric and -0.3 <= x <= 3: `logmar`
#'   - if x numeric and all x in intersection(va_chart$logMAR, va_chart$snellen_dec):
#'     `logmar`, but you can choose `snellen`
#'
#' Detection and conversion is on a vector as a whole by [which_va()].
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
#' If smallstep = TRUE, each snellen optotype will be considered
#' equivalent to 0.02 logmar or 1 ETDRS letter (assuming 5 letters
#' in a row in a chart)
#' @section Mixed entries:
#' If Snellen entries (containing "/") and numeric values are found, the notation
#' will be guessed as "mixed". This behaviour can be turned off with "mixed =
#' FALSE". In this case, "snellen" will be chosen as default unless otherwise
#' specified with the from argument.
#' Note that some plausibility checks may be overruled with mixed notation.
#' @section VA cleaning:
#' For more details see [clean_va()]
#' 1. `NA` is assigned to strings such as "." or "", "n/a" or "   "
#' 1. notation for qualitative entries is simplified.
#' @section VA classes:
#' convert_VA returns a vector of three classes:
#'   1. `va`
#'   1. One of snellen, snellendec, logmar, etdrs or quali.
#'   1. Either of `character` (for Snellen, snellendec, and qualitative),
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
va <- function(x, from = NULL, to = NULL, type = "ft",
               smallstep = FALSE, noplus = FALSE) {
  if (!is.atomic(x)) {
    stop("x must be atomic", call. = FALSE)
  }

  if (is.null(to)) {
    return(clean_va(x))
  } else {
    to <- tolower(to)
    if (length(to) != 1 | !to %in% c("etdrs", "logmar", "snellen")) {
      message("'to' must be 'etdrs', 'logmar', or 'snellen'")
      return(x)
    } else if (to == "snellen"){
      if(!type %in% c("ft", "m", "dec")) {
        message("'type' must be 'ft', 'm' or 'dec'. picking ft")
        type <- "ft"
      }
    } else {
      type <- NULL
    }
  }
  if (inherits(x, "va")) {
    set_va <- class(x)[class(x) %in% VAclasses]
    if (to == set_va) {
      message("VA already in desired class. No conversion made")
      return(x)
    } else {
      va_class <- set_va
    }
  } else {
    x <- clean_va(x, message = FALSE)
    guess_va <- which_va(x)

    if (all(guess_va %in% "NA")) {
      message("No conversion - vector of NA")
      return(x)
    }

    if (!is.null(from)) {
      if (length(from) == 1) {
        from <- tolower(from)
        if (!from %in% guess_va) {
          message(paste0(
            "This doesn't look like ", from, ". Could be ",
            paste(guess_va, collapse = ", "), ". Picking ", guess_va[1]
          ))
          va_class <- guess_va[1]
        } else {
          va_class <- from
        }
      }
    } else {
      if (length(guess_va) == 1 &
          all(guess_va %in% VAclasses)) {
        message(paste("From", guess_va))
        va_class <- guess_va
      } else if (length(guess_va) > 1) {
        message(paste0(
          "From ", guess_va[1], ". Could be ", paste(guess_va, collapse = ", ")
        ))
        va_class <- guess_va[1]
      }
    }
  }
  x_noquali <- convertQuali(x, to_class = va_class)
  class(x_noquali) <- va_class
  x_plausible <- checkVA(x_noquali)
  # return(list(x_noquali, x_plausible))
  x_final <-
    convertVA(x_plausible, to = to, type = type,
              smallstep = smallstep, noplus = noplus)
  # return(list(va_class, to, type, x_final))
  x_final
}
# va(x, to = "logmar")
# va(x, to = "snellen", from = "etdrs")
# xx <- va(x, to = "snellen", from = "snellendec")

#' VA classes
#' @name VAclasses
#' @description eye introduces classes for VA notation.
#' If the class is not manually attributed to the vectors (not advised!), they
#' will have (at least) three classes:
#'   1. class "va"
#'   1. One of class 'snellen', 'snellendec', 'logmar', 'etdrs' or 'quali'
#'   1. Either of 'character' (for Snellen, snellendec, and qualitative),
#'       'numeric' (for logMAR), or 'integer' (for ETDRS).
#' @keywords internal
VAclasses <- c("logmar", "snellen", "snellendec", "etdrs", "quali")

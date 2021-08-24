#' Visual acuity notation conversion
#' @description Cleans and converts visual acuity notations (classes)
#'   between Snellen (decimal, meter and feet), ETDRS, and logMAR.
#' @param x Vector with visual acuity entries. Must be atomic.
#' Snellen fractions need to be entered with "/"
#' @param from will force to evaluate from which notation to convert - Must be
#'   "etdrs", "logmar", "snellen" or "snellendec".
#'   Ignored if the value should not be plausible.
#' @param to To which class to convert. "etdrs", "logmar" or "snellen" -
#'   any case allowed. If NULL (default), will simply "clean up" VA entries.
#'   This may then result in a vector of "mixed" VA notations.
#' @param type To which Snellen notation to convert: "m", "dec" or "ft"
#' @param smallstep how +/- entries are evaluated. FALSE:
#'   increase/decrease Snellen fractions by lines. TRUE: plus/minus
#'   entries equivalent to 0.02 logmar
#' @param noplus ignoring plus/minus entries and just returning the
#'     snellen fraction. This overrides the smallstep argument.
#' @name va
#' @section VA conversion:
#' - **logMAR to ETDRS**: logMAR rounded to the first digit and converted with
#' the visual acuity chart (see section VA chart)
#' - **Snellen to logMAR**: logMAR = -1 * log10(snellen_frac)
#' - **Snellen to ETDRS**: ETDRS = 85 + 50 * log10(snellen_frac)
#' \doi{10.1097/iae.0b013e3181d87e04}
#' - **ETDRS to logMAR**: logMAR = -0.02 * etdrs + 1.7
#' Beck et al. \doi{10.1016/s0002-9394(02)01825-1}
#' - **Hand movements and counting fingers** are converted following
#' Schulze-Bonsel et al. - https://doi.org/10.1167/iovs.05-0981
#' - **(No) light perception** are converted following the suggestions by
#' [Michael Bach](https://michaelbach.de/sci/acuity.html)
#' @section Qualitative visual acuity entries:
#' In order to calculate with qualitative entries counting fingers,
#' hand movement and (no) perception of light, **use logMAR** !
#' Qualitative visual acuity lower than counting fingers is assigned 0
#' ETDRS letter, in order to keep it as a measurement (not: NA). It is very
#' difficult to justify a "negative" letter score in a test which only has
#' a specific range (0-100).
#'
#' - **To Snellen**:
#' Although there seems to be no good statistical reason to convert
#' back to Snellen, it is a very natural thing to eye specialists to think
#' in Snellen. A conversion to snellen gives a good gauge of how the visual
#' acuity for the patients are. However, back-conversion should not be
#' considered an exact science and any attempt to use formulas will result
#' in very weird Snellen values that have no correspondence to common charts.
#' Therefore, Snellen matching the nearest ETDRS and logMAR value in
#' the VA chart are used.
#' @section VA chart:
#' You can find with eye:::va_chart.
#' This chart and VA conversion formulas are based on charts in
#' Holladay et al.\doi{10.1016/j.jcrs.2004.01.014}, Beck et al.
#' \doi{10.1016/s0002-9394(02)01825-1}{Beck et al.}, and
#' Gregori et al.\doi{10.1097/iae.0b013e3181d87e04}.
#' The etdrs values for NLP and PL are deliberately set at those values because
#' they are unlikely to happen by chance as a wrong entry (and as integers),
#' and it has internal reasons that make conversion easier.
#' @section Accepted VA formats / Plausibility checks:
#' - Snellen fractions (meter/ feet) need to be entered as fraction with
#' "/". Any fractions allowed. You can get creative with your snellens.
#' see **"Examples"**
#' - ETDRS must be integer-equivalent between 0 and 100 (integer equivalent
#' means, it can also be a character vector)
#' - logMAR must be -0.3 <= x <= 3.0
#' - Snellen decimal must be 0 < x <= 2
#' - Qualitative must be either of PL, LP, NLP, NPL, HM, CF (any case allowed)
#' - Plausibility checks are performed for the automatically or manually defined
#' notation.
#' - Any element which is implausible/ not recognized will be converted to NA
#' @section Entries with mixed VA notations:
#' Use [va_mixed] instead.
#' @section Snellen "+/-" entries:
#' By default, plus/minus entries are evaluated as intended by the
#' test design: Snellen fractions increase/decrease only by lines.
#'
#'     - if entry -2 to +2 : take same Snellen value
#'     - if < -2 : take Snellen value one line below
#'     - if > +2 : take Snellen value one line above
#'
#' If smallstep = TRUE, each snellen optotype will be considered
#' equivalent to 0.02 logmar (assuming 5 letters in a row in a chart)
#' @section VA cleaning:
#' For more details see [clean_va()]
#' 1. `NA` is assigned to strings such as "." or "", "n/a" or "   "
#' 1. notation for qualitative entries is simplified.
#' @section VA classes:
#' convertVA returns a vector of three classes:
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
    } else if (to == "snellen") {
      if (!type %in% c("ft", "m", "dec")) {
        message("'type' must be 'ft', 'm' or 'dec'. picking ft")
        type <- "ft"
      }
    } else {
      type <- NULL
    }
  }
  if (inherits(x, "va")) {
    conVA <- convertVA(x, to = to, type = type,
                       smallstep = smallstep, noplus = noplus
    )
      return(conVA)
    } else {
    x_clean <- clean_va(x, message = FALSE)
    guess_va <- which_va(x_clean)

    if (all(guess_va %in% "NA")) {
      message("No conversion - vector of NA")
      return(x_clean)
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
  class(x_clean) <- c(va_class, class(x_clean))

  x_noquali <- convertQuali(x_clean, to_class = va_class)

  class(x_noquali) <- va_class

  x_plausible <- checkVA(x_noquali)
  newNA <- as.logical(is.na(x_plausible) - is.na(x))
  introduceNA(x, newNA)

  x_final <- convertVA(
    x_plausible, to = to, type = type,
    smallstep = smallstep, noplus = noplus
  )
  x_final
}

#' VA classes
#' @name va_mixed
#' @param x vector with mixed VA entries
#' @param to to which notation to be converted
#' @param possible which possible VA notations - and the precedence given,
#'     see details
#' @description va_mixed is a wrapper around [va] on all possible VA notations.
#'   By default, c("snellen", "etdrs", "logmar", "snellendec") will be converted -
#'   in that order! For tricky cases see details and examples. Note
#'   that va_mixed will not give nice messages which values are transformed
#'   from which notation, and which values were replaced with NA.
#' @details Mixed entries are challenging, but unfortunately seem to occur in
#'   real life data. It will be fairly individual what you have in yours, but
#'   it should hopefully not happen that you have *all* possible notations.
#'   Snellen fractions are usually not challenging because they contain a "/",
#'   thus are easy to recognize.
#'
#'   **Most problematic are values between 0 and 3**,
#'   in particular full integers - this can be EDTRS, snellen decimal notation
#'   or logmar. If your data doesn't have snellen decimal notation,
#'   specify this with "possible", e.g. with
#'   `possible = c("snellen", "etdrs", "logmar")`. If you know that you don't
#'   have any ETDRS value less than 4, you can safely give precedence to logmar
#'   instead, like this: `possible = c("snellen", "logmar", "etdrs")`
#'   @examples
#'   # awfully mixed notation!! (and note the wrong -1 value)
#'   x <- c(NA, "nlp", 1:2, 1.1, -1, "20/40", "4/6", "6/1000", 34)
#'   va_mixed(x, to = "snellen")
#'
#'   # "I only have snellen and snellen decimal notation in my data"
#'   va_mixed(x, to = "snellen", possible = c("snellen", "snellendec"))
#'
#'   # "I have snellen, logmar and etdrs in my data, and there is no etdrs value
#'   less than 4"
#'   va_mixed(x, to = "snellen", possible = c("snellen", "logmar", "etdrs"))
#' @family Ophthalmic functions
#' @family VA converter
#' @export
va_mixed <- function(x, to, possible) {
  if (missing(possible)) {
    possible <- VAclasses[1:4]
  } else {
    possible <- tolower(possible)
  }
  a <- sapply(possible, function(vaclass) {
    suppressMessages(va(x, to = to, from = vaclass))
  })
  apply(a, 1, function(z) z[!is.na(z)][1])
}

#' VA classes
#' @name VAclasses
#' @keywords internal
VAclasses <- c( "snellen", "etdrs", "logmar", "snellendec", "quali")

#' Visual acuity entry cleaner
#' @name clean_va
#' @param x Vector with VA entries
#' @description VA cleaning:
#' 1. [isNAstring()]:
#'   Replacing empty placeholders (".","", "(any number of empty space)",
#'   "NULL", "NA", "N/A" ) - any cases - with NA
#' 1. Removing "plus" and "minus" from snellen notation
#'     - if entry -1 to +3 : take same Snellen value
#'     - if <= -2 : take Snellen value one line below
#'     - if >+3 (unlikely, but unfortunately not impossible):
#'     Snellen value one line above
#' 1. [convert_NLP()] Simplifying the notation for qualitative VA notation
#' (NPL becomes NLP, PL becomes LP)
#' @family VA cleaner
#' @export
clean_va <- function(x){
  x[isNAstring(x)] <- NA_character_
  ## if "/" (meaning snellen notation), then removing plus/minus entries
  x <- gsub("\\s","", x)
  if(any(grepl("\\/", x)) & any(grepl("\\+|-", x))){
  x <- parse_plusminus(x)
  }
  convert_NLP(x)
}

#' @rdname clean_va
#' @param replace_PL named vector how to rename qualitative VA
#' @family VA cleaner
convert_NLP <- function(x, replace_PL = c(pl = "lp", npl = "nlp")) {
  x <- tolower(x)
  new_vec <- replace_PL[x]
  unname(ifelse(is.na(new_vec), x, new_vec))
}

#' @rdname clean_va
#' @param full vector of full strings to be replaced by NA
isNAstring <- function(x, full = c("\\.+", "", "\\s+", "n/a", "na", "null")) {
  x <- tolower(x)
  full <- paste0("^", paste(full, collapse = "$|^"), "$")
  ismissing <- grepl(full, x)
  ismissing
}


#' @rdname clean_va
#' @section parse_plusminus:
#' Snellen are unfortunately often entered with "+/-", which is a
#' violation of a psychophysical method designed to assign one
#' unambiguous value to visual acuity, with
#' non-arbitrary thresholds based on psychometric functions. Therefore,
#' transforming "+/-" notation to actual results is in itself
#' problematic and the below suggestion to convert it will remain an
#' approximation to the most likely "true" result. Even more so, as the
#' given conditions should work for charts with
#' 4 or 5 optotypes in a line, and visual acuity is not always tested
#' on such charts. Yet, I believe that the approach is still better than
#' just omitting the letters or (worse) assigning a missing value to those
#' entries.
#' @seealso
#' https://en.wikipedia.org/wiki/Psychometric_function

parse_plusminus <- function(x){
  x_split <- strsplit(x, "(?<=.)(?=[-\\+])", perl = TRUE)
  parsed_elem <- sapply(x_split, function(x) {
    snellen_split <- x[1]
    plusminus <- suppressWarnings(as.integer(x[2]))
    snellen_type <- if (snellen_split %in% va_chart$snellen_ft) {
      "snellen_ft"
    } else if (snellen_split %in% va_chart$snellen_m) {
      "snellen_m"
    } else {
      NA
    }
    if (plusminus <= 3 & plusminus >= -1 |
        is.na(plusminus) | is.na(snellen_type)) {
      new_elem <- snellen_split
    } else if (plusminus <- 1) {
      ind <- match(snellen_split, va_chart[[snellen_type]]) - 1
      new_elem <- va_chart[[snellen_type]][ind]
    } else if (plusminus > 3) {
      ind <- match(snellen_split, va_chart[[snellen_type]]) + 1
      new_elem <- va_chart[[snellen_type]][ind]
    }
    new_elem
  })
  parsed_elem
}

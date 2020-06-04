#' Visual acuity entry cleaner
#' @name va_cleaner
#' @param x Vector with VA entries
#' @description Internal functions for VA cleaning.
#'
#' `va_cleaner`: wrapper around [convert_na_va], [remove_plus], and
#' [convert_NLP]. Assigns `NA` to missing entries, removes "plus" and "minus"
#'  from snellen notation  and simplifies the notation for qualitative VA notation.
#' @family VA cleaner
clean_va <- function(x){
  x_nona <- convert_na_va(x)
  x_noplus <- remove_plus(x_nona)
  x_noNLP <- convert_NLP(x_noplus)
  x_noNLP
}


#' convert_na_va
#' @rdname va_cleaner
#' @description `convert_na_va`: wrapper around [isNAstring]

convert_na_va <- function(x){
  x[isNAstring(x)] <- NA_character_
  x
}

#' convert_NLP
#' @rdname va_cleaner
#' @param replace_PL named vector how to rename qualitative VA

convert_NLP <- function(x, replace_PL = c(pl = "lp", npl = "nlp")) {
  x <- tolower(x)
  new_vec <- replace_PL[x]
  unname(ifelse(is.na(new_vec), x, new_vec))
}

#' remove_plus
#' @rdname va_cleaner
#' @importFrom stringr str_extract

remove_plus <- function(x) {
  x <- stringr::str_extract(x, "(-)?\\w+[.\\w/]*(?=-?)")
  x
}

#' isNAstring
#' @rdname va_cleaner
#' @param full vector of full strings to be replaced by NA
#' @param partial vector of partial strings to be replaced by NA

isNAstring <- function(x, full = c("\\.+", "", "\\s+", "n/a", "na", "null"), partial = c("not")) {
  if (is.numeric(x) | is.integer(x)) {
    stop("x is numeric/integer. No NA strings expected", call. = FALSE)
  }
  x <- tolower(x)
  partial <- paste(partial, collapse = "|")
  full <- paste0("^", paste(full, collapse = "$|^"), "$")

  full_partial <- paste(c(partial, full), collapse = "|")
  ismissing <- grepl(full_partial, x)
  ismissing
}


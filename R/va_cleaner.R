#' convert_na_va
#' @name va_cleaner
#' @param x vector
#' @family va cleaner functions
#'
convert_na_va <- function(x){
  x[isNAstring(x)] <- NA_character_
  x
}

#' convert_NLP
#' @rdname va_cleaner
#' @param x vector
#' @family va cleaner functions
#'
convert_NLP <- function(x, replace_PL = c(PL = "LP", NPL = "NLP")) {
  new_vec <- replace_PL[as.character(x)]
  unname(ifelse(is.na(new_vec), x, new_vec))
}

#' remove_plus
#' @rdname va_cleaner
#' @param x vector
#' @importFrom stringr str_extract
#' @family va cleaner functions
#'
remove_plus <- function(x) {
  x <- stringr::str_extract(x, "(-)?\\w+[.\\w/]*(?=-?)")
  x
}

#' clean_va
#' @rdname va_cleaner
#' @param x vector
#' @family va cleaner functions
clean_va <- function(x){
  x_nona <- convert_na_va(x)
  x_noplus <- remove_plus(x_nona)
  x_noNLP <- convert_NLP(x_noplus)
  x_noNLP
}

#' isNAstring
#' @rdname va_cleaner
#' @param x vector
#' @family va cleaner functions
#'
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


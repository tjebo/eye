#' Visual acuity entry cleaner
#' @name clean_va
#' @param x Vector with VA entries
#' @description VA cleaning:
#' 1. [isNAstring()]:
#'   Replacing empty placeholders (".","", "(any number of empty space)",
#'   "NULL", "NA" ) with NA with
#' 1. Removing "plus" and "minus" from snellen notation
#' 1. [convert_NLP()] Simplifying the notation for qualitative VA notation
#' (NPL becomes NLP, PL becomes LP) .
#' @importFrom stringr str_extract
#' @family VA cleaner
#' @export

clean_va <- function(x){
  x[isNAstring(x)] <- NA_character_
  ## if "/" (meaning snellen notation), then removing plus/minus entries
  if(any(grepl("\\/", x))){
  x <- stringr::str_extract(x, "(-)?\\w+[.\\w/]*(?=-?)")
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


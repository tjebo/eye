#' Visual acuity entry cleaner
#' @name clean_va
#' @param x Vector with VA entries
#' @description VA cleaning:
#' 1. [isNAstring()]:
#'   Replacing empty placeholders (".","", "(any number of empty space)",
#'   "NULL", "NA", "N/A" ) - any cases - with NA
#' 1. [convert_NLP()] Simplifying the notation for qualitative VA notation
#' (NPL becomes NLP, PL becomes LP)
#' @family VA cleaner
#' @export
clean_va <- function(x){
  x <- tolower(as.character(x))
  x[isNAstring(x, tolower = FALSE)] <- NA_character_
  x <- gsub("\\s","", x)
   convert_NLP(x, tolower = FALSE)

   x_quali <- x %in% unlist(set_codes()["quali"])
   x_snell <- grepl("/", x)
   x_keep <- as.logical (x_quali + x_snell)
   x_num <- suppressWarnings(as.numeric(x_noquali))

   if (all(is.na(x_num))) {
     return("failed")
   } else if (all(grepl("/", x_noquali[!is.na(x_noquali)]))) {
     return("snellen")
}
}
#' @rdname clean_va
#' @param replace_PL named vector how to rename qualitative VA
#' @param tolower if TRUE, x will be converted to lower first
#' @family VA cleaner
convert_NLP <- function(x, replace_PL = c(pl = "lp", npl = "nlp"),
                        tolower = TRUE) {
    if(tolower){
      x <- tolower(x)
    }
  new_vec <- replace_PL[x]
  unname(ifelse(is.na(new_vec), x, new_vec))
}

#' @rdname clean_va
#' @param full vector of full strings to be replaced by NA
#' @param tolower if TRUE, x will be converted to lower first
isNAstring <- function(x, full = c("\\.+", "", "\\s+", "n/a", "na", "null"),
                       tolower = TRUE) {
  if(tolower){
    x <- tolower(x)
  }
  full <- paste0("^", paste(full, collapse = "$|^"), "$")
  ismissing <- grepl(full, x)
  ismissing
}

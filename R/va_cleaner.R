#' Cleaning up Visual acuity entries
#' @name clean_va
#' @param x Vector with VA entries
#' @param quali strings for qualitative visual acuity entries
#' @description VA cleaning:
#' 1. [isNAstring()]:
#'   Replacing empty placeholders (".","", "(any number of empty space)",
#'   "NULL", "NA", "N/A" , "-") - any cases - with NA
#' 1. [convert_NLP()] Simplifying the notation for qualitative
#' VA notation (NPL becomes NLP, PL becomes LP)
#' 1. Removing non-Snellen character strings
#' @return character vector
#' @family VA cleaner
#' @export
clean_va <- function(x, quali = c("nlp", "lp", "hm", "cf")) {
  originalNA <- is.na(x)
  x_tidied <- tolower(tidyNA(x))
  x_tidied <- gsub("\\s", "", x_tidied)

  # unifying nlp/npl etc
  replace_PL = c(pl = "lp", npl = "nlp")
  new_vec <- replace_PL[x_tidied]
  x_tidied <- unname(ifelse(is.na(new_vec), x_tidied, new_vec))

  x_quali <- x_tidied %in% quali
  x_snell <- grepl("/", x_tidied)
  x_num <- !is.na(suppressWarnings(as.numeric(x_tidied)))
  x_keep <- as.logical(x_quali + x_snell + x_num + originalNA)

  introduceNA(x, !x_keep)

  if (sum(x_quali, x_snell) == 0) {
    return(as.numeric(x_tidied))
  }
  x_tidied
}

#' introduce NA for implausible VA entries
#' @name introduceNA
#' @param x vector
#' @param test plausibility test
#' @return vector
#' @keywords internal
introduceNA <- function(x, test){
  if(any(test)){
    message(paste0(
      sum(test, na.rm = TRUE), "x NA introduced for: ",
      paste(unique(x[test]), collapse = ", ")
    ))
  }
}

#' Internal clean for va()
#' @keywords internal
clean_short <- function(x) {
  x_tidied <- gsub("\\s", "", tolower(x))
  replace_PL <- c(pl = "lp", npl = "nlp")
  new_vec <- replace_PL[x_tidied]
  x <- unname(ifelse(is.na(new_vec), x_tidied, new_vec))
  x
}

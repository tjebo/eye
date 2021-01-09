
#' convert quali entries
#' @description S3 methods for converting quality VA entries
#' @name convertQuali
#' @param x vector
#' @param to_class to which class
#' @return vector
#' @keywords internal

convertQuali <- function(x, to_class){
  if(to_class == "snellen"){
    to_class <- "snellenft"
  }
  if (any(x %in% c("nlp", "lp", "hm", "cf"))) {
    x_quali <- va_chart[[to_class]][match(x, va_chart$quali[1:4])]
    x <- ifelse(!is.na(x_quali), x_quali, x)
  }
  x
}




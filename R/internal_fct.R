#' whole
#' @rdname internals
#' @description internal unction factory for function
#'   to match "whole string" with any non character as boundaries
#'   accepts string as regular expression.
#' @family string matching functions
#' @examples
#' whole("eye")(amd)
#'
whole <- function(string){
  reg <-  paste0("(?<![a-z])(", paste(tolower(string), collapse = "|"), ")(?![a-z])")

function(x){
  if(inherits(x, "data.frame")){
    cols <- colnames(x)
  } else {
    cols <- x
  }
  cols[grepl(reg, tolower(cols), perl = TRUE)]
}
}

#' partial
#' @rdname internals
#' @description internal unction factory for function to match string
#'   accepts string as regular expression.
#' @family string matching functions
#' @examples
#' partial("pat|Id")(amd)

partial <- function(string){
  function(x){
    if(inherits(x, "data.frame")){
    cols <- colnames(x)
    } else {
      cols <- x
    }
    cols[grepl(paste(tolower(string), collapse = "|"), tolower(cols), perl = TRUE)]
  }
}

#' eyecols
#' @rdname internals
#' @description internal function gets the eye columns
#' @family string matching functions
#'
eyecols <- function(x) {
  eye_r <- c("r", "re", "od", "right")
  eye_l <- c("l", "le", "os", "left")
  col_r <- whole(eye_r)(x)
  col_l <- whole(eye_l)(x)
  list(r = col_r, l = col_l)
}

#' va_cols
#' @rdname internals
#' @description internal function gets the va columns
#' @family string matching functions
va_cols <- function(eyecols) {
  va_cols <- lapply(eyecols, function(x) {
    paste0(whole(c("va", "bcva"))(x), partial("acuit")(x))
  })
  va_cols
}

#' iop_cols
#' @rdname internals
#' @description internal function gets the va columns
#' @family string matching functions
#'
iop_cols <- function(eyecols) {
  iop_cols <- lapply(eyecols, function(x) {
    paste0(whole("iop")(x), partial("pressure")(x))
  })
  iop_cols
}



#' Your data in a blink of an eye
#' @description High level function, wrapping [myop], [eyes] and [reveal]
#' @name blink
#' @param x data frame
#' @details Steps of data processing:
#'  - checking for duplicate rows and removing them
#'  - Renaming columns to prepare for myopization and VA conversion
#'  - VA will always be converted to logmar by default
#' @examples
#' TBC
#' @export

blink <- function(x, id = NULL, eye = NULL, to = "logmar", ...) {
  x <- myop(x)
  eye_cols <- unlist(eye:::getElem_eye(x))
  va_cols <- eye:::getElem_va(x)
  if (length(va_cols) < 1){
    message("No VA column detected")
  } else {
    x <- dplyr::mutate_at(x, .vars = unname(va_cols),
                          .funs = va, to = to)
  }

  res_count <- eyes(x, ...)

  res_va <- reveal(x[va_cols])

  if(length(eye_cols) > 0){
    ls_va_eyes <- lapply(split(x, x$eye), function(x) reveal(x[va_cols]))
    res_va_eyes <- do.call(rbind, ls_va_eyes)
  } else {
    res_va_eyes <- NULL
  }
  ls_blink <- c(list(data = x,
                   count = res_count,
                   VA_total = res_va),
                   VA_eyes = res_va_eyes)

  class(ls_blink) <- c("blink", class(ls_blink))
  ls_blink
}
# blink(amd)
# blink(abraham)
# ab_new <- myop(abraham)

#' Your data in a blink of an eye
#' @description High level function, wrapping [myop], [eyes] and [reveal]
#' @name blink
#' @param x data frame
#' @param id passed to [eyes]
#' @param eye passed to [eyes]
#' @details Steps of data processing:
#'  - checking for duplicate rows and removing them
#'  - Renaming columns to prepare for myopization and VA conversion
#'  - VA will always be converted to logmar by default
#' @examples
#' blink(amd)
#'
#' messy_df <- data.frame( id = letters[1:3],
#' iop_r_preop = sample(21:23), iop_r_postop = sample(11:13),
#' iop_l_postop = sample(11:13), iop_l_preop = sample(31:33),
#' va_r_preop = sample(41:43),  va_l_preop = sample(41:43),
#' va_r_postop = sample(51:53), va_l_postop = sample(45:47)
#' )
#' blink(messy_df)

#' @export

blink <- function(x, id = NULL, eye = NULL) {
  x <- myop(x)
  eye_cols <- whole_str(c("eyes", "eye"))(names(x))
  va_cols <- getElem_va(x)
  if (length(va_cols) < 1){
    message("No VA column detected")
  } else {
    x <- dplyr::mutate_at(x, .vars = unname(va_cols), .funs = va)
  }

  res_count <- eyes(x, id = id, eye = eye)

  res_va <- reveal(x[va_cols])

  if(length(eye_cols) > 0){
    res_va_eyes <- reveal(x[c(eye_cols, va_cols)], by = eye_cols)
  } else {
    res_va_eyes <- NULL
  }
  ls_blink <- append(list(data = x,
                   count = res_count,
                   VA_total = res_va),
                   list(VA_eyes = res_va_eyes))

  class(ls_blink) <- c("blink", class(ls_blink))
  ls_blink
}

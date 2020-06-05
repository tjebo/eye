#' Your data in a blink of an eye
#' @description High level function, wrapping [myop], [eyes] and [reveal]
#' @name blink
#' @param x data frame
#' @param id passed to [eyes]
#' @param eye passed to [eyes]
#' @param fct Keep columns for reveal and va that are factors
#' @param fct_level Remove columns for reveal and va when all unique values
#' fall into the range of fct_level
#' @param binary Remove columns for reveal and va with logicals
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

blink <- function(x, id = NULL, eye = NULL,
                  fct = TRUE, fct_level = 0:4, binary = TRUE) {
  x <- myop(x)
  eye_cols <- whole_str(c("eyes", "eye"))(names(x))
  va_cols <- getElem_va(x)
  rm_va_cols <- remCols(x = x, va_cols, fct = fct,
                        fct_level = fct_level, binary = binary)
  va_cols <- va_cols[rm_va_cols]
  iop_cols <- getElem_iop(x)

  if (length(va_cols) < 1){
    message("No VA column detected")
    res_va <- NULL
    res_va_eyes <- NULL
  } else {
    x <- dplyr::mutate_at(x, .vars = unname(va_cols), .funs = va)
    res_va <- reveal(x[va_cols])
    if(length(eye_cols) > 0){
      res_va_eyes <- reveal(x[c(eye_cols, va_cols)], by = eye_cols)
    } else{
      res_va_eyes <- NULL
    }
  }
  if (length(iop_cols) < 1){
    res_iop <- NULL
    res_iop_eyes <- NULL
  } else {
    res_iop <- reveal(x[iop_cols])
    if(length(eye_cols) > 0){
      res_iop_eyes <- reveal(x[c(eye_cols, iop_cols)], by = eye_cols)
    }else{
      res_va_eyes <- NULL
    }
  }
  res_count <- eyes(x, id = id, eye = eye)
  ls_blink <- append(list(data = x,
                   count = res_count),
                   list(VA_total = res_va, VA_eyes = res_va_eyes,
                        IOP_total = res_iop, IOP_eyes = res_iop_eyes))

  class(ls_blink) <- c("blink", class(ls_blink))
  ls_blink
}

#' Remove cols from selected cols
#' @param x data frame
#' @param cols cols
#' @param fct Keep columns for reveal and va that are factors
#' @param fct_level Remove columns for reveal and va when all unique values
#' fall into the range of fct_level
#' @param binary Remove columns for reveal and va with logicals
#' @description avoiding colums that only
#' contain values fct_levels or binary,
#' because they are likely categorical codes.
#' @rdname remCols
remCols <- function(x, cols, fct, fct_level, binary) {
  sapply(cols, function(col) {
  y <- x[[col]]
  if(!fct & is.factor(y)){
    FALSE
  }
  if(all(unique(y) %in% fct_level)){
    FALSE
  } else if(isTRUE(binary) & all(unique(y) %in% c(TRUE, FALSE))){
    FALSE
  } else {
    TRUE
  }
  }
, USE.NAMES = FALSE)
}



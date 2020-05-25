#' long eye data
#' @description Wrapper around [tidyr::pivot_longer()] to make your data
#' long ("myopic")
#' @name myop
#' @param x data frame
#' @param ... arguments passed to [tidyr::pivot_longer()]
#' @details
#' - **cols**: if not specified, `myop()` will look for columns that have
#' typical eye codes, i.e. c("r", "re" and "od") for right eyes and
#' c("l", "le" and "os") for left eyes.
#' - if argument specified, right eye has to be given first!
#' - **eye_code**: numeric codes of 0:1 or 1:2 are allowed, other
#' numeric codes are not supported.
#' @import tidyr
#' @importFrom rlang expr
#' @importFrom dplyr full_join
#' @examples
#' set.seed(42)
#' iop <- data.frame(id = letters[1:11], r = sample(10:20), l = sample(10:20))
#'
#' myop(iop, values_to = "iop")
#'
#' # Example to clean a bit messy data frame
#' iop_va <- data.frame(id = letters[1:11],
#'                      iop_r = sample(10:20), iop_l = sample(10:20),
#'                      va_r = sample(40:50), va_l = sample(40:50))
#' myop(iop_va)
#'
#' @export

myop <- function(x, ...) {

  eye_r <- set_eye()$r
  eye_l <- set_eye()$l
  eye_code <- c("r", "l")

  ls_eye <- get_eyecols(x)
  va_cols <- get_va_cols(ls_eye)
  leng_va <- lengths(va_cols)
  iop_cols <- get_iop_cols(ls_eye)
  leng_iop <- lengths(iop_cols)

  if (all(lengths(ls_eye) < 1)) {
    stop("No columns found for gathering", call. = FALSE)
  }

  message(paste0("Picked \"", paste(ls_eye[[1]], collapse = ","), "\" and \"", paste(ls_eye[[2]], collapse = ","), "\" for right and left eyes"))

  if (any(leng_va > 1) | any(leng_iop > 1)) {
    stop("Too many VA and/or IOP columns - don't know how to gather", call. = FALSE)
  }
  if (all(leng_va < 1) & all(leng_iop < 1)) {
    if (any(lengths(ls_eye) > 1)) {
      stop("Too many eye columns - don't know how to gather", call. = FALSE)
    }
    message("Neither VA nor IOP column(s) found. Gathering eye columns")
    eye_cols <- unlist(ls_eye)
    names(x)[names(x) %in% eye_cols] <- eye_code
    cols <- rlang::expr(c(eye_code[1], eye_code[2]))
    res_df <- tidyr::pivot_longer(x, cols = !!cols, names_to = names_to, values_to = values_to, ...)
  } else if (all(leng_va == 1) & all(leng_iop == 1)) {
    message("Gathering both VA and IOP columns")
    if (missing(id)) {
      id <- part_str(c("pat", "id"))(colnames(x))
    }
    if (length(id) != 1) {
      stop("Patient column missing. Don't know how to gather. Fix with \"id\" argument", call. = FALSE)
    }
    eye_cols_iop <- unlist(iop_cols)
    eye_cols_va <- unlist(va_cols)
    x_iop <- x[which(!names(x) %in% eye_cols_va)]
    names(x_iop)[names(x_iop) %in% eye_cols_iop] <- eye_code
    cols <- rlang::expr(c(eye_code[1], eye_code[2]))
    iop_long <- tidyr::pivot_longer(x_iop, cols = !!cols, names_to = names_to, values_to = "IOP", ...)

    x_va <- x[which(!names(x) %in% eye_cols_iop)]
    names(x_va)[names(x_va) %in% eye_cols_va] <- eye_code
    cols <- rlang::expr(c(eye_code[1], eye_code[2]))
    va_long <- tidyr::pivot_longer(x_va, cols = !!cols, names_to = names_to, values_to = "VA", ...)
    res_df <- dplyr::full_join(iop_long, va_long, by = c(id, "eye"))
  } else if (any(leng_va < 1) & all(leng_iop == 1)) {
    message("Gathering IOP columns")
    eye_cols <- unlist(iop_cols)
    names(x)[names(x) %in% eye_cols] <- eye_code
    cols <- rlang::expr(c(eye_code[1], eye_code[2]))
    res_df <- tidyr::pivot_longer(x, cols = !!cols, names_to = names_to, values_to = "IOP")
  } else if (all(leng_va == 1) & any(leng_iop < 1)) {
    message("Gathering VA columns")
    eye_cols <- unlist(va_cols)
    names(x)[names(x) %in% eye_cols] <- eye_code
    cols <- rlang::expr(c(eye_code[1], eye_code[2]))
    res_df <- tidyr::pivot_longer(x, cols = !!cols, names_to = names_to, values_to = "VA", ...)
  }
  class(res_df) <- c("myop", "tbl", "tbl_df", "data.frame")
  res_df
}

#' myope
#' @rdname myop
#' @export
myope <- myop

#' myopise
#' @rdname myop
#' @export
myopise <- myop

#' myopize
#' @rdname myop
#' @export
myopize <- myop

#' myopic
#' @rdname myop
#' @export
myopic <- myop

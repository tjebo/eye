#' long eye data
#' @description Wrapper around [tidyr::pivot_longer()] to make your data
#' long ("myopic")
#' @name myop
#' @param x data frame
#' @param id if there are both IOP and VA columns, one needs to
#'   specify if there is more than one ID column.
#' @param ... arguments passed to [tidyr::pivot_longer()]
#' @details
#' `myop()` will look for columns that have
#' typical eye codes, i.e. c("r", "re", "od", "right") for right eyes and
#' c("l", "le", "os", "left") for left eyes.
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

myop <- function(x, id = NULL, ...) {
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

  message(paste0(
    "Picked \"", paste(ls_eye[[1]], collapse = ","),
    "\" and \"", paste(ls_eye[[2]], collapse = ","),
    "\" for right and left eyes"
  ))

  if (any(leng_va > 1) | any(leng_iop > 1)) {
    stop("cannot do this yet") # gather VA and IOP separately and rejoin data frames,
    # create column with "variable spec"
  }
  if (all(leng_va < 1) & all(leng_iop < 1)) {
    if (any(lengths(ls_eye) > 1)) {
      stop("Many eye columns and no VA/IOP column -
           gather failed. If you want to make your data longer,
           clean your data and/or use tidyr::pivot_longer", call. = FALSE)
    }
    message("Neither VA nor IOP column(s) found. Gathering eye columns")
    res_df <- pivot_myop(x, ls_eye, values_to = "value")
  } else if (all(leng_va == 1) & all(leng_iop == 1)) {
    message("Gathering both VA and IOP columns")
    if (missing(id)) {
      id <- part_str(c("pat", "id"))(colnames(x))
    }
    if (length(id) != 1) {
      stop("Patient column missing. How to gather?
           Fix with \"id\" argument, or clean your data
           and/or use tidyr::pivot_longer", call. = FALSE)
    }

    x_iop <- x[which(!names(x) %in% unlist(va_cols))]
    x_va <- x[which(!names(x) %in% unlist(iop_cols))]

    iop_long <- pivot_myop(
      x = x_iop, old_cols = iop_cols,
      names_to = "eye", values_to = "IOP"
    )
    va_long <- pivot_myop(
      x = x_va, old_cols = va_cols,
      names_to = "eye", values_to = "VA"
    )
    res_df <- dplyr::full_join(iop_long, va_long, by = c(id, "eye"))
  } else if (any(leng_va < 1) & all(leng_iop == 1)) {
    message("Gathering IOP columns")
    res_df <- pivot_myop(x = x, old_cols = iop_cols, values_to = "IOP")
  } else if (all(leng_va == 1) & any(leng_iop < 1)) {
    message("Gathering VA columns")
    res_df <- pivot_myop(x = x, old_cols = va_cols, values_to = "VA")
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

#' pivot_myop#'
#' @rdname myop
#' @param x vector
#' @family myopizing functions
#'

pivot_myop <- function(x, old_cols, eye_code = c("r","l"), ...){
  eye_cols <- unlist(old_cols)
  names(x)[names(x) %in% eye_cols] <- eye_code
  cols <- rlang::expr(c(eye_code[1], eye_code[2]))
  res_df <- tidyr::pivot_longer(x, cols = !!cols, ...)
  res_df
}



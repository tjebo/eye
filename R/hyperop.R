#' Hyperopic eye data
#' @description Pivot eye-related variables to two columns
#' @name hyperop
#' @param x data frame
#' @param cols columns which should be made "wide". Tidyselection supported
#' @param eye eye column (default looking for "eye" or "eyes", all cases)
#' @return A tibble, see also [tibble::tibble]
#' @details
#' Basically the opposite of [myop()] - a slightly intelligent
#' wrapper around [tidyr::pivot_longer()] and [tidyr::pivot_wider()]
#' Will find the eye column, unify the codes for the eyes (all to "r" and "l")
#' and pivot the columns wide, that have been specified in "cols".
#'
#' **Good names and tidy data always help!**
#'
#' For more information about shaping data and good names, see `vignette("eye")`,
#' or `?blink` or `?myop`
#'
#' @examples
#' # Example to clean a bit messy data frame
#'
#' iopva <- data.frame(
#'   id = c("a", "e", "j", "h"),
#'   va_r = c(37L, 36L, 33L, 38L),
#'   iop_r = c(38L, 40L, 33L, 34L),
#'   va_l = c(30L, 39L, 37L, 40L),
#'   iop_l = c(31L, 34L, 33L, 31L)
#' )
#' myop_iop <- myop(iopva)
#' hyperop(myop_iop, cols = matches("va|iop"))
#'
#'
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr unite
#' @importFrom rlang enquo
#' @importFrom rlang !!
#' @seealso
#' [About tidyselection](
#' https://tidyselect.r-lib.org/reference/language.html)
#' @export

hyperop <- function(x, cols, eye = NULL) {
  if (is.null(eye)) {
    eye <- whole_str(c("eyes", "eye"))(colnames(x))
  }
  if (length(eye) != 1) {
    warning("Eye column must be specified. Did not change data.
            Use argument \"eye\"", call. = FALSE)
    return(x)
  }
  x[[eye]] <- recodeye(x[[eye]])
  cols_exp <- rlang::enquo(cols)

  # va_index <- unname(tidyselect::eval_select(x_exp, x))
  x_long <- tidyr::pivot_longer(x, cols = !!cols_exp,
                                names_to = "newkey", values_to = "newvalue")
  x_unite <- tidyr::unite(x_long, col = "eye_newkey", c(eye, "newkey"))
  x_spread <- tidyr::pivot_wider(x_unite, names_from = "eye_newkey",
                                 values_from = "newvalue")
  x_spread
}



#' reveal
#' @name reveal
#' @description Shows commonly used summary statistics
#' @param x data frame, numeric vector, or list of numeric vectors
#' @param by character vector with the names of the columns
#' @param dec how many decimals are displayed
#' @importFrom dplyr bind_rows
#' @return data frame
#' @details Character vectors (or character columns) will be removed.
#' @family reveal
#' @examples
#' x = y = z = c(rnorm(20), NA)
#' mylist <- list(x = x, y = y, z = z)
#' ## vectors
#' reveal(x)
#' reveal(1:10)
#' ## named or unnamed list
#' reveal(mylist)
#' set.seed(42)
#' mydf <- cbind(group = rep(letters[1:3], 4),
#' setNames(as.data.frame(replicate(c(rnorm(11), NA), n = 3)), letters[24:26]))
#' ## data frames
#' reveal(mydf)
#' ## data frames by group
#' reveal(mydf, by = "group")
#' @export
reveal <- function(x, by = NULL, dec = 1) {
  if (!is.null(by)) {
    x <- reveal_split(x, by = by)
  }
  revealEye(x, by = by, dec = dec)
}

#' reveals little helper
#' @name reveal_methods
#' @param x atomic vector, list of numeric vectors or data frame
#' @param ... further arguments passed to methods
#' @family reveal
#' @description S3 generic and methods
#' @importFrom stats sd
#' @return data frame
#' @export
revealEye <- function(x, ...){
  funs_eyeReveal <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE),
    n = function(x) length(x[!is.na(x)]),
    min = function(x) min(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE)
  )
  UseMethod("revealEye", x)
}

#' @rdname reveal_methods
#' @param by grouping variable - character vector with column names
#'   can be several variables!
#' @param dec how many decimals are displayed
#' @export
revealEye.list <- function(x, by, dec, ...) {
  if (is.null(by)) {
    by <- "group"
  }
  ls_x <- lapply(x, revealEye, dec = dec)
  new_col <- paste(by, collapse = "_")
  res <- dplyr::bind_rows(ls_x, .id = new_col)
  res_split <- split_mult(res, new_col, into = by)
  res_split
}

#' @rdname reveal_methods
#' @param dec how many decimals are displayed
#' @export
revealEye.numeric <- function(x, dec, ...){
  res <- lapply(funs_eyeReveal, function(f) round(f(x), dec))
  unlist(res)
}

#' @rdname reveal_methods
#' @param dec how many decimals are displayed
#' @export
revealEye.data.frame <- function(x, dec, ...) {
  x <- as.data.frame(x)
  x[1:length(x)] <-
    suppressWarnings(as.numeric(as.character(unlist(x[1:length(x)]))))
  x_num <- Filter(f = function(x) {
    sum(is.na(x)) != length(x)
  }, x = x)
  if (!identical(x, x_num)) {
    warning("reveal: character elements removed", call. = FALSE)
  }
  result <- lapply(funs_eyeReveal, mapply, x_num)
  list_res <- lapply(result, function(y) round(y, digits = dec))
  x <- data.frame(list_res)
  x <- cbind(var = rownames(x), data.frame(x, row.names=NULL))
  x
}

#' @rdname reveal_methods
#' @export
revealEye.default <- function(x, dec, ...){
  NULL
}
#' reveals little helper
#' @name reveal_split
#' @param x data frame
#' @param by character vector with the names of the columns
#' @description splits into groups
#' @family reveal
reveal_split <- function(x, by){
  if(!inherits(x, "data.frame")){
    stop("x must be a data frame!")
  }
  x_sym <- deparse(substitute(x))
  group <- paste0("interaction(",
                  paste0(x_sym, "$", by, collapse = ", "),
                  ", sep = \"_\")")
  ls_x <- split(x[base::setdiff(names(x), by)], eval(parse(text = group)))
  ls_x
}



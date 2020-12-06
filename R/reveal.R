#' reveal
#' @name reveal
#' @description Shows commonly used summary statistics
#' @param x data frame, numeric vector, or list of numeric vectors
#' @param by character vector with the names of the columns. Can be
#' several variables!
#' @param dec how many decimals are displayed
#' @param funs not really meant to be used at the moment - change the
#'  Summarizing functions with a named(!) list of functions
#' @importFrom dplyr bind_rows
#' @return data frame
#' @details Character vectors (or character columns) will be removed.
#' @family revealer
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
reveal <- function(x, by = NULL, dec = 1, funs = NULL) {
  if (is.null(funs)) {
    funs <- list(
      mean = function(x) mean(x, na.rm = TRUE),
      sd = function(x) sd(x, na.rm = TRUE),
      n = function(x) length(x[!is.na(x)]),
      min = function(x) min(x, na.rm = TRUE),
      max = function(x) max(x, na.rm = TRUE)
    )
  }
  if (!is.null(by)) {
    x <- reveal_split(x, by = by)
  }
  revealEye(x, by = by, dec = dec, funs = funs)
}

#' reveals little helper
#' @name reveal_methods
#' @inheritParams reveal
#' @param ... further arguments passed to methods
#' @family revealer
#' @description S3 generic and methods
#' @importFrom stats sd
#' @return data frame
#' @export
revealEye <- function(x, ...){
  UseMethod("revealEye", x)
}

#' @rdname reveal_methods
#' @export
revealEye.list <- function(x, by, dec, funs, ...) {
  if (is.null(by)) {
    by <- "group"
  }
  ls_x <- lapply(x, revealEye, dec = dec, funs = funs)
  new_col <- paste(by, collapse = "_")
  res <- dplyr::bind_rows(ls_x, .id = new_col)
  res_split <- split_mult(res, new_col, into = by)
  res_split
}

#' @rdname reveal_methods
#' @export
revealEye.numeric <- function(x, dec, funs, ...){
  res <- lapply(funs, function(f) round(f(x), dec))
  unlist(res)
}

#' @rdname reveal_methods
#' @export
revealEye.data.frame <- function(x, dec, funs, ...) {
  x <- as.data.frame(x)
  x[1:length(x)] <-
    suppressWarnings(as.numeric(as.character(unlist(x[1:length(x)]))))
  x_num <- Filter(f = function(x) {
    sum(is.na(x)) != length(x)
  }, x = x)
  if (!length(x) == length(x_num)) {
    warning("reveal: character elements removed", call. = FALSE)
  }
  result <- lapply(funs, mapply, x_num)
  list_res <- lapply(result, function(y) round(y, digits = dec))
  x <- data.frame(list_res)
  x <- cbind(var = rownames(x), data.frame(x, row.names=NULL))
  x
}


#' @rdname reveal_methods
#' @export
revealEye.default <- function(x, dec, funs, ...){
  NULL
}

#' reveals little helper
#' @name reveal_split
#' @param x data frame
#' @param by character vector with the names of the columns
#' @description splits into groups
#' @family revealer
#' @keywords internal
reveal_split <- function(x, by){
  if(!inherits(x, "data.frame")){
    stop("x must be a data frame!")
  }
  x_sym <- deparse(substitute(x))
  group <- paste0("interaction(",
                  paste0(x_sym, "$", by, collapse = ", "),
                  ", sep = \"_\", drop = TRUE)")
  ls_x <- split(x[base::setdiff(names(x), by)], eval(parse(text = group)))
  ls_x
}

#' split columns in multiple by regex
#' @name split_mult
#' @description Reveal helper. cuts column into multiple in reveal
#' @param x data frame
#' @param col character
#' @param pattern regex by which to split
#' @param into names of columns - character vector of length of n splits
#' @param prefix if into not specified, names created "prefix(sep)index"
#' @param sep separator of prefix and index
#' @importFrom stringr str_split_fixed
#' @keywords internal
#' @seealso
#' modified from
#' [this thread on stackoverflow](https://stackoverflow.com/a/47060452/7941188)
split_mult <- function(x, col,
                       pattern = "_",
                       into = NULL,
                       prefix = "var",
                       sep = ""){
  cols <- stringr::str_split_fixed(x[[col]], pattern, n = Inf)
  cols[which(cols == "")] <- NA_character_
  m <- dim(cols)[2]
  if(length(into) == m){
    colnames(cols) <- into
  } else {
    colnames(cols) <- paste(prefix, 1:m, sep = sep)
  }
  cbind(cols, x[names(x) != col])
}


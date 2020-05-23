#' whole
#' @rdname internals
#' @description internal unction factory for function
#'   to match "whole string" with any non character as boundaries
#'   accepts string as regular expression.
#' @family string matching functions
#' @examples
#' whole("eye")(amd)
#'
whole <- function(string) {
  reg <- paste0("(?<![a-z])(", paste(tolower(string), collapse = "|"), ")(?![a-z])")
  function(x) {
    x[grepl(reg, tolower(x), perl = TRUE)]
  }
}

#' part_str
#' @rdname internals
#' @description internal unction factory for function to match string
#'   accepts string as regular expression.
#' @family string matching functions
#' @examples
#' part_str("pat|Id")(amd)

part_str <- function(string) {
  function(x) {
    x[grepl(paste(tolower(string), collapse = "|"), tolower(x), perl = TRUE)]
  }
}

#' set eye terms
#' @rdname internals
#' @description set the eye strings looked for in the data
#' @family string matching functions
set_eye <- function(r = c("r", "re", "od", "right"), l = c("l", "le", "os", "left")){
  list(r = r, l = l)
}

#' set iop terms
#' @rdname internals
#' @description set the iop strings looked for in the data
#' @family string matching functions
set_iop <- function(whole = c("iop", "gat", "nct"), partial = "pressure"){
  list(whole = whole, partial = partial)
}
#' set va terms
#' @rdname internals
#' @description set the va strings looked for in the data
#' @family string matching functions
set_va <- function(whole = c("va", "bcva"), partial = "acuit"){
  list(whole = whole, partial = partial)
}

#' IDcols
#' @rdname internals
#' @param obj can be vector, data frame or list
#' @param id_str strings to identify id colums. Default c("id", "pat")
#' for convenience [set_eye] function can and should be used.
#' @description internal function gets the eye columns
#' @family string matching functions
#'
get_idcols <- function(obj, id_str = c("id", "pat")) {
  if (inherits(obj, "data.frame")) {
    obj <- colnames(obj)
  }
  pat_id <- part_str("(pat.*id|id.*pat)")(obj)
  if (length(pat_id) > 0) {
    pat_col <- pat_id
  } else {
    pat_col <- part_str(c("pat", "id"))(obj)
  }
  pat_col
}
#' eyecols
#' @rdname internals
#' @param obj can be vector, data frame or list
#' @param eye_chr named list with strings for "r" and "l"
#' names have to be "r" and "l"!
#' for convenience [set_eye] function can and should be used.
#' @description internal function gets the eye columns
#' @family string matching functions
#'
get_eyecols <- function(obj, eye_chr) {
  if(missing(eye_chr)){
    eye_chr <-  set_eye()
  }
  if (inherits(obj, "data.frame")) {
    obj <- colnames(obj)
    ls_eye <- lapply(eye_chr, function(x) whole(x)(obj))
  } else if (is.atomic(obj)){
    ls_eye <- lapply(eye_chr, function(x) whole(x)(obj))
  } else {
    stop("only atomic or data.frame supported")
  }
  ls_eye
}

#' iop_cols
#' @rdname internals
#' @param obj can be vector, data frame or list
#' @param iop_chr named list with strings for "whole" and "partial"
#' names have to be "whole" and "partial"!
#' for convenience [set_iop] function can and should be used.
#' @description internal function gets the iop columns
#' @family string matching functions
#'
get_iop_cols <-  function(obj, iop_chr) {
  if(missing(iop_chr)){
    iop_chr <-  set_iop()
  }
  if (inherits(obj, "data.frame")) {
    ns_obj <- colnames(obj)
    iop_cols <- paste0(whole(iop_chr$whole)(ns_obj), part_str(iop_chr$partial)(ns_obj))
  } else if (is.atomic(obj)) {
    iop_cols <- paste0(whole(iop_chr$whole)(obj), part_str(iop_chr$partial)(obj))
  } else {
    iop_cols <- lapply(obj, function(x) {
      paste0(whole(iop_chr$whole)(x), part_str(iop_chr$partial)(x))
    })
  }
  iop_cols
}

#' va_cols
#' @rdname internals
#' @param obj can be vector, data frame or list
#' @param va_chr named list with strings for "whole" and "partial"
#' names have to be "whole" and "partial"!
#' for convenience [set_va] function can and should be used.
#' @description internal function gets the va columns
#' @family string matching functions
#'
get_va_cols <- function(obj, va_chr) {
  if(missing(va_chr)){
    va_chr <-  set_va()
  }
  if (inherits(obj, "data.frame")) {
    ns_obj <- colnames(obj)
    va_cols <- paste0(whole(va_chr$whole)(ns_obj), part_str(va_chr$partial)(ns_obj))
  } else if (is.atomic(obj)) {
    va_cols <- paste0(whole(va_chr$whole)(obj), part_str(va_chr$partial)(obj))
  } else {
    va_cols <- lapply(obj, function(x) {
      paste0(whole(va_chr$whole)(x), part_str(va_chr$partial)(x))
    })
  }
  va_cols
}


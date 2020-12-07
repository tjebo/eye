#' Find element based or strings
#' @name getElem
#' @param obj can be vector, data frame or list
#' @param id_chr strings to identify ID colums.
#' @description Finds the element in an object for one of the following:
#'  - ID
#'  - eye
#'  - Visual acuity
#'
#' `getElem` search a vector, the column names (data frame) or the names (list)
#' @keywords internal
#' @return vector
#' @family string matching functions
#'
getElem_id <- function(obj, id_chr) {
  if(missing(id_chr)){
    id_chr <-  unlist(set_codes()["id"])
  }
  if (inherits(obj, "data.frame")) {
    obj <- colnames(obj)
  }
  both_elem <- both_str(id_chr)(obj)
  if (length(both_elem) > 0) {
    pat_col <- both_elem
  } else if (any(grepl("^id$", obj))){
    pat_col <- grep("^id$", obj, value = TRUE)
  } else {
    pat_col <- part_str(id_chr)(obj)
  }
  pat_col
}

#' @name getElem
#' @param obj can be vector, data frame or list
#' @param eye_chr strings to identify ID colums.
#' @description Finds the element in an object for one of the following:
#'  - ID column
#'  - eye column
#'  - columns that contain information for right/left eyes
#'  - Visual acuity
#'
#' `getElem` search a vector, the column names (data frame) or the names (list)
#' @keywords internal
#' @return vector
#' @family string matching functions
#'
getElem_eyecol <- function(obj) {
  if (inherits(obj, "data.frame")) {
    obj <- colnames(obj)
  }

  if (any(grepl("(?<![a-z])(eye|eyes)(?![a-z])", obj, perl = TRUE,
                ignore.case = TRUE))){
    eye_col <- whole_str(c("eye","eyes"))(obj)
  } else {
    eye_col <- part_str("eye")(obj)
  }
  eye_col
}

#' @rdname getElem
#' @param eye_chr named list with strings for "r" and "l"
#' names have to be "r" and "l"!
getElem_eye <- function(obj, eye_chr) {
  if(missing(eye_chr)){
    eye_chr <-  set_codes()[c("r","l")]
  }
  if (inherits(obj, "data.frame")) {
    obj <- colnames(obj)
    ls_eye <- lapply(eye_chr, function(x) whole_str(x)(obj))
  } else if (is.atomic(obj)){
    ls_eye <- lapply(eye_chr, function(x) whole_str(x)(obj))
  } else {
    stop("only atomic or data.frame supported")
  }
  ls_eye
}

#' @rdname getElem
#' @param va_chr named list with strings for "whole" and "part"
#' names have to be "whole" and "part"!
getElem_va <- function(obj, va_chr) {
  if(missing(va_chr)){
    va_chr <-  list(whole = c("va", "bcva", "etdrs", "snellen", "logmar"), part = "acuit")
  }
  if (inherits(obj, "data.frame")) {
    ns_obj <- colnames(obj)
    va_cols <- paste0(whole_str(va_chr$whole)(ns_obj), part_str(va_chr$part)(ns_obj))
  } else if (is.atomic(obj)) {
    va_cols <- paste0(whole_str(va_chr$whole)(obj), part_str(va_chr$part)(obj))
  } else {
    va_cols <- lapply(obj, function(x) {
      paste0(whole_str(va_chr$whole)(obj), part_str(va_chr$part)(obj))
    })
  }
  va_cols
}

#' @rdname getElem
#' @param iop_chr named list with strings for "whole" and "part"
#' names have to be "whole" and "part"!
getElem_iop <- function(obj, iop_chr) {
  if(missing(iop_chr)){
    iop_chr <-  list(whole = c("iop", "gat", "nct"), part = "pressure")
  }
  if (inherits(obj, "data.frame")) {
    ns_obj <- colnames(obj)
    iop_cols <- paste0(whole_str(iop_chr$whole)(ns_obj), part_str(iop_chr$part)(ns_obj))
  } else if (is.atomic(obj)) {
    iop_cols <- paste0(whole_str(iop_chr$whole)(obj), part_str(iop_chr$part)(obj))
  } else {
    iop_cols <- lapply(obj, function(x) {
      paste0(whole_str(iop_chr$whole)(obj), part_str(iop_chr$part)(obj))
    })
  }
  iop_cols
}

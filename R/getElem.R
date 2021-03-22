#' Find element based or strings
#' @name getElem
#' @param obj can be vector, data frame or list
#' @description Finds the element in an object for one of the following:
#'  - *getElem_id* ID column
#'  - *getElem_eyecol* eye column
#'  - *getElem_eye* columns that contain information for right/left eyes
#'  - *getElem_va* Visual acuity
#'  - *getElem_iop* IOP
#'
#' `getElem` search a vector, the column names (data frame) or the names (list)
#' @keywords internal
#' @return vector
#' @family string matching functions
getElem_id <- function(obj) {
  id_chr <-  eye_codes$id
  both_elem <- both_str(obj, id_chr)
  if (length(both_elem) > 0) {
    pat_col <- both_elem
  } else if (any(grepl("^id$", obj))){
    pat_col <- grep("^id$", obj, value = TRUE)
  } else {
    pat_col <- part_str(obj, id_chr)
  }
  pat_col
}

#' @rdname getElem
getElem_eyecol <- function(obj) {
  eye_chr <- eye_codes$eye
  eye_chr_coll <- paste0("^(", paste(eye_chr, collapse = "|"),")$")
  is_eye_col <- obj[grepl(eye_chr_coll, tolower(obj))]
  if(length(is_eye_col) > 0){
    eye_col <- is_eye_col
  } else if (length(whole_str(obj, eye_chr)) > 0){
    eye_col <- whole_str(obj, eye_chr)
  } else {
    eye_col <- part_str(obj, eye_chr)
  }
  eye_col
}

#' @rdname getElem
getElem_eye <- function(obj) {
  eye_chr <-  eye_codes[c("right","left")]
  lapply(eye_chr, function(x) whole_str(obj, x))
}

#' @rdname getElem
getElem_va <- function(obj) {
  va_chr <-
    list(
      whole = unlist(eye_codes[c("va", "va_method")], use.names = FALSE),
      part = eye_codes$va_partial
    )
  va_cols <- paste0(whole_str(obj, va_chr$whole), part_str(obj, va_chr$part))
  va_cols
}

#' @rdname getElem
getElem_iop <- function(obj) {
  iop_chr <- list(whole = eye_codes$iop, part = eye_codes$iop_partial)
  iop_cols <- paste0(whole_str(obj, iop_chr$whole), part_str(obj, iop_chr$part))
  iop_cols
}

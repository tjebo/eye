#' Your data in a blink of an eye
#' @description `blink` summarizes your data tailored to the need of
#' ophthalmic research: It looks for VA and IOP columns and summarises those
#' with common statistics. In order to make it work, it requires specific
#' column naming - please see section "column names" and "data coding".
#' For more details how blink works, see `vignette("eye")`
#' @name blink
#' @param x data frame
#' @param va_to to which VA notation (passed to [va()])
#' @param va_cols if specified, overruling automatic VA columns selection.
#'   tidyselection supported
#' @param iop_cols if specified, overruling automatic IOP columns selection.
#'   tidyselection supported
#' @param fct_level Remove columns for Summarizing when all unique values
#' fall into range. character or numeric vector, default `1:4`
#' @details `blink` is basically a wrapper around [myop], [eyes] and [reveal]:
#' - Duplicate rows are always removed
#' - Column names are prepared for myopization (see [myop])
#' - VA will always be converted to logmar
#' @section Data coding:
#' - Only common codes supported:
#' - **eyes**: "r", "re", "od", "right" - or numeric coding r:l = 0:1 or 1:2
#' - **Visual acuity**: "VA", "BCVA", "Acuity"
#' - **Intraocular pressure**: "IOP", "GAT", "NCT", "pressure"
#' @section Column name rules:
#' - No spaces!
#' - Do not use numeric coding for eyes in column names
#' - Separate eye and VA and IOP codes with **underscores**
#' ("bcva_l_preop", "VA_r", "left_va", "IOP_re")
#' - Avoid separate VA or IOP codes if this is not actually containing
#' VA/ IOP data (e.g. "stableVA" instead of "stable_va", ChangeIOP instead
#' of "change_IOP")
#' - Keep names short
#' - Don't use underscores when you don't have to.
#' Consider each section divided by an underscore as a relevant
#' characteristic of your variable. ("preop" instead of "pre_op",
#' "VA" instead of "VA_ETDRS_Letters")
#' - Use common codes for your patient column (see [eyes], section Guessing)
#' (e.g., "pat", "patient" or "ID", ideally both: "patientID" or "patID")
#' - **Don't be too creative with your names!**
#' @section Names examples:
#' **Good names**:
#'
#' -`c("patid", "surgery_right", "iop_r_preop", "va_r_preop", "iop_r", "iop_l")`
#'
#' **OK names**
#'
#' -`c("Id", "Eye", "BaselineAge", "VA_ETDRS_Letters", "InjectionNumber")`:
#' Names are long and there are two unnecessary underscore in the VA column.
#' Better just "VA"
#' -`c("id", "r",  "l")`: All names are commonly used (good!),
#' but which dimension of "r"/"l" are we exactly looking at?
#'
#' **Bad names** (`eye` will fail)
#'
#' - `c("id", "iopr", "iopl", "VAr", "VAl")`:
#' `eye` won't be able to recognize IOP and VA columns
#' - `c("id", "iop_r", "iop_l", "stable_iop_r", "stable_iop_l")`:
#' `eye` *may* wrongly identify the (probably logical) columns "stable_iop"
#' as columns containing IOP data. Better maybe: "stableIOP_l"
#' - `c("person", "goldmann", "vision")`: `eye` will not recognize that at all
#' @section tidy data:
#'  **blink and myop work more reliably with clean data**
#'  (any package will, really!).
#'  [clean data.](https://tidyr.tidyverse.org/articles/tidy-data.html)
#' @section column removal:
#' Done with [remCols]: Removes columns that only
#' contain values defined in *fct_levels* or logicals from selected columns
#' (currently for both automatically and manually selected columns).
#' fct_levels are removed because they are likely categorical codes.
#' @importFrom dplyr mutate_at
#' @importFrom rlang enquo
#' @importFrom tidyselect eval_select
#' @importFrom tibble as_tibble
#' @return object of class `blink` and `list`. Class blink contains the
#'   myopized data, count of patients and eyes,
#'   and summaries for visual acuities and intraocular pressure.
#' @seealso [About tidyselection](https://tidyselect.r-lib.org/reference/language.html).
#'
#' How to rename your columns (two threads on stackoverflow.com):
#' - [Rename columns 1](https://stackoverflow.com/questions/7531868/how-to-rename-a-single-column-in-a-data-frame)
#' - [Rename columns 2](https://stackoverflow.com/questions/20987295/rename-multiple-columns-by-names/59567220#59567220)
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

blink <- function(x, va_to = "logmar",
                  va_cols = NULL, iop_cols = NULL,
                  fct_level = 0:4) {
  if(!rlang::quo_is_null(rlang::enquo(va_cols))){
      x_exp <- rlang::enquo(va_cols)
      va_index <- unname(tidyselect::eval_select(x_exp, x))
  } else {
    va_index <- which(names(x) %in% getElem_va(x))
  }
  if(!rlang::quo_is_null(rlang::enquo(iop_cols))){
    x_exp <- rlang::enquo(iop_cols)
    iop_index <- unname(tidyselect::eval_select(x_exp, x))
  } else {
    iop_index <- which(names(x) %in% getElem_iop(x))
  }

  names(x) <- myop_rename(x)
  x_myop <- myopizer(x)

  eye_cols <- whole_str(c("eyes", "eye"))(names(x_myop))

  if (length(va_index) < 1){
    message("No VA column detected")
    res_va <- NULL
    res_va_eyes <- NULL
  } else {
    va_false <- remCols(x = x, cols = va_index, fct_level = fct_level)
    va_true <- va_index[va_false]
    names_va <- names(x)[va_true]
    new_names_va <- unique(gsub("^(r|l)_", "", names_va))
    #update VA with VA
    x_myop <- dplyr::mutate_at(x_myop, new_names_va, .funs = va, to = va_to)
    # summary for VA cols (based on new names!)
    res_va <- reveal(x_myop[new_names_va])
    if(length(eye_cols) > 0){
      res_va_eyes <-
        reveal(x_myop[c(eye_cols, new_names_va)], by = eye_cols)
    } else {
      res_va_eyes <- NULL
    }
  }
  if (length(iop_index) < 1){
    res_iop <- NULL
    res_iop_eyes <- NULL
  } else {
    iop_false <- remCols(x = x, cols = iop_index, fct_level = fct_level)
    iop_true <- iop_index[iop_false]
    names_iop <- names(x)[iop_true]
    new_names_iop <- unique(gsub("^(r|l)_", "", names_iop))
    res_iop <- reveal(x_myop[new_names_iop])
    if(length(eye_cols) > 0){
      res_iop_eyes <-
      reveal(x_myop[c(eye_cols, new_names_iop)], by = eye_cols)
    } else {
      res_iop_eyes <- NULL
    }
  }
  x_myop <- tibble::as_tibble(x_myop)

  res_count <- eyes(x_myop)

  ls_blink <- Filter(x = list(data = x_myop, count = res_count,
                   VA_total = res_va, VA_eyes = res_va_eyes,
                   IOP_total = res_iop, IOP_eyes = res_iop_eyes),
                   f = function(x) !is.null(x))

  class(ls_blink) <- c("blink", class(ls_blink))
  ls_blink
}

#' Remove cols from selected cols
#' @param x data frame
#' @param cols cols
#' @param fct_level Remove columns for reveal and va when all unique values
#' fall into the range of fct_level
#' @description Helper for [blink()]. Removes columns that only
#' contain values fct_levels or binary from selected columns (currently for
#' both automatically and manually selected columns).
#' fct_levels are removed because they are likely categorical codes.
#' @keywords internal
#' @return logical vector
#' @rdname remCols
remCols <- function(x, cols, fct_level) {
  sapply(cols, function(col) {
    y <- x[[col]]
    y_new <- tolower(suppressWarnings(as.character(y)))

    if (all(y_new[!is.na(y_new)] %in% unlist(set_codes()["quali"]))) {
      return(TRUE)
    }
    y_noquali <- y_new[!y_new %in% unlist(set_codes()["quali"])]
    y_num <- suppressWarnings(as.numeric(y_noquali))

    if (any(grepl("/", y_noquali))) {
      res <- TRUE
    } else if (all(is.na(y_num)) |
      all(unique(y_new) %in% c("true", "false"))) {
      return(FALSE)
    }
    if (all(unique(y_new) %in% tolower(fct_level))) {
      res <- FALSE
    } else {
      res <- TRUE
    }
    res
  },
  USE.NAMES = FALSE
  )
}

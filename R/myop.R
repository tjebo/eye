#' Myopic eye data
#' @description Pivot "eye" variable to one column
#' @name myop
#' @param x data frame
#' @param var Character vector of length 1 specifying the variable if there
#'  is only one column per eye with no further info on the variable
#'  (default "value")
#' @details
#' Out of convenience, data is often entered in a very "wide" format:
#' there will be two columns for the same variable, one column for each eye.
#' myop will pivot the eye variable to one column and keep all other
#' variables wide. E.g., eight columns that store data of four variables for two eyes
#' will be pivoted to 5 columns (one eye and four further variable columns,
#' see also *examples*).
#'
#' **myop requires a specific data format**
#'
#' If there is a column called "eye" or "eyes", myop will not make
#' any changes - because the data is then already assumed to be in long
#' format. If you neverthess have columns with eye-specific values,
#' then you have messy data. Maybe, you could remove or rename the "eye"
#' column and then let myop do the work.
#'
#' myop will only recognize meaningful coding for eyes:
#' - Right eyes: *"r", "re", "od", "right"*
#' - Left eyes:  *"l", "le", "os", "left"*
#' - for other codes see also [set_codes]
#' The strings for eyes need to be **separated by period or underscores**.
#' (Periods will be replaced by underscores). Any order is allowed.
#'
#' - **Will work**: "va_r", "right_morningpressure", "night_iop.le", "gat_os_postop"
#' - **Will fail**: "VAr", "rightmorningPressure", "night_IOPle", "gatOSpostop"
#'
#' An exception is when there is only one column for each eye. Then
#' the column names can consist of eye strings (see above) only.
#' In this case, *var* will be used to "name" the resulting variable.
#'
#' If there are only eye columns in your data (should actually not happen),
#' myop will create identifiers by row position.
#'
#' **Please always check the result for plausibility.**
#' Depending a lot on how the data was entere, the results could become
#' quite surprising. There is basically a nearly infinite amount of
#' possible combinations of how to enter data, and it is likely that
#' myop will not be able to deal with all of them
#' @examples
#' # Example to clean a bit messy data frame
#' iopva <- data.frame(
#'   id = c("a", "e", "j", "h"),
#'   va_r = c(37L, 36L, 33L, 38L),
#'   iop_r = c(38L, 40L, 33L, 34L),
#'   va_l = c(30L, 39L, 37L, 40L),
#'   iop_l = c(31L, 34L, 33L, 31L)
#' )
#' myop(iopva)
#'
#' @export

myop <- function(x, var = "value") {
  names(x) <- myop_rename(x)
  myopizer(x, var = var)
}

#' @rdname myop
#' @export
myope <- myop

#' myopic
#' @rdname myop
#' @export
myopic <- myop

#' myopizer
#' @description The actual myopization using tidyr::pivot_longer
#' several columns with partial strings with "eye codes" in their names
#' @name myop_pivot
#' @param x object (data frame)
#' @import tidyr
#' @importFrom dplyr mutate_all
#' @family myopizer

myop_pivot <- function(x) {
  x_myop <- x %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::pivot_longer(
      cols = tidyr::matches("^(r|l)_"),
      names_to = "eyekey",
      values_to = "new_val_wow"
    ) %>%
    tidyr::extract("eyekey", regex = "^(r|l)_(.*)", into = c("eye", "eyekey")) %>%
    tidyr::pivot_wider(names_from = "eyekey", values_from = "new_val_wow")

  x_myop
}

#' Rename
#' @description Rename data names for Myop
#' @name myop_rename
#' @param x object (data frame)
#' @import tidyr
#' @importFrom dplyr mutate_all
#' @family myopizer

myop_rename <- function(x) {
  if (any(grepl("\\.", names(x)))) {
    names(x) <- gsub("\\.", "_", names(x))
  }
  name_x <- sort_substr(
    tolower(names(x)),
    set_codes()[base::setdiff(
      names(set_codes()),
      c("id", "quali", "method")
    )]
  )
  name_x
}


#' myopizer
#' @description checks iand prepares data frames for myopization
#' Removing duplicates, returning myopized data if criteria fulfilled
#' (No "eye" column, more than one variable column with eye codes
#' as partial string). Names need to be prepared with
#' [myop_rename]) beforehand.
#' @name myopizer
#' @inheritParams myop
#' @import tidyr
#' @importFrom dplyr mutate_all
#' @family myopizer
myopizer <- function(x, var = "value") {
  x_sym <- deparse(substitute(x))
  ls_eye <- getElem_eye(x)
  eye_cols <- unlist(ls_eye)
  eye_str <- whole_str(c("eyes", "eye"))(names(x))

  if (anyDuplicated(x)) {
    which_dupe <- paste(which(duplicated(x) | duplicated(x, fromLast = TRUE)),
                        collapse = ","
    )
    warning(paste0(
      "Removed duplicate rows from data (rows ", which_dupe, ")"
    ), call. = FALSE)
    x <- x[!duplicated(x), ]
  }

  if (all(lengths(ls_eye) == 1) & !all(grepl("_", eye_cols))) {
    names(x)[names(x) %in% eye_cols] <- paste(eye_cols, var, sep = "_")
  }
  if (any(lengths(ls_eye) < 1) | length(eye_str > 0) | !any(grepl("_", names(x)))) {
    warning("Data seems already myopic - no changes made. ?myop for help",
            call. = FALSE
    )
    return(x)
  }

  if (length(names(x)) == length(eye_cols)) {
    x$id <- seq_len(nrow(x))
  }
  myop_pivot(x)
}


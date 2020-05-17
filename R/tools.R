#' eyes
#' @name eyes
#' @description Looks for columns that identify patients and eyes and counts number of patients and eyes.
#' @param date dataframe where patient / eye information is included.
#' @param id id column. If not specified, automatically selected. In this case, the patient ID column name needs to contain the strings "pat" OR "id" ignore.case = TRUE.
#' @param eye eye colum. If not specified, automatically selected, in which case the eye column name needs to contain the string "eye" ignore.case = TRUE
#' @param text default = FALSE (TRUE will return text which can be pasted for example into a markdown document). As default a named vector will be returned
#' @examples
#' set.seed(42)
#' foo <- data.frame(id = c(letters[sample(10)], letters[sample(10)] ), eyes = c("r","l"))
#' eyes(foo)
#' #'
#' set.seed(42)
#' foo1 <- data.frame(id = c(letters[sample(10)], letters[sample(10)] ), eyes = c(NA,"l"))
#' set.seed(42)
#' foo2<- data.frame(id = c(letters[sample(10)], letters[sample(10)] ), eyes = c("r","l"))
#' set.seed(42)
#' foo3 <- data.frame(id = c(letters[sample(10)], letters[sample(10)] ), eyes = c("e","l"))
#' set.seed(42)
#' foo4 <- data.frame(id = c(letters[sample(10)], letters[sample(10)] ), eyes = c("od","le"))
#' set.seed(42)
#' foo5 <- data.frame(patient= c(letters[sample(10)], letters[sample(10)] ), eyes = c("od","le"))
#' set.seed(42)
#' foo6 <- data.frame(patient= c(letters[sample(10)], letters[sample(10)] ), eyes = c("od","le", "re", "le"))
#' set.seed(42)
#' foo7 <- data.frame(patient= c(letters[sample(10)], letters[sample(10)] ), eyes = c("od","le"), patience = 'c')
#' set.seed(42)
#' foo8 <- data.frame(patient= c(letters[sample(10)], letters[sample(10)] ), eyes = c("od","le", "re", "os"))
#'
#' purrr::map(purrr::map(paste0("foo", 1:8), get), function(x) try(eyes(x)))
#'
#' eyes(amd)
#' @export
#'
eyes <- function(data, id = NULL, eye = NULL, text = FALSE) {
  x <- eval(data)

  if (missing(id)) {
    pat_col <- names(x)[grepl("pat|id", names(x), ignore.case = TRUE)]
  } else {
    pat_col <- id
  }

  if (missing(eye)) {
    eye_col <- names(x)[grepl("eye", names(x), ignore.case = TRUE, perl = TRUE)]
  } else {
    eye_col <- eye
  }

  if (length(eye_col) < 1 | identical(eye_col, character(0))) {
    warning("No eye column found in data nor specified: No eye count.", call. = FALSE)
  }

  if (length(pat_col) < 1) {
    stop("Patient and/or eye column(s) are missing.\n
         The patient ID column name needs to contain either of or both strings
         \"pat\" and \"ID\" at any location.\n
         The eye column name needs to contain the string \"eye\"
         Cases can be ignored.", call. = FALSE)
  }

  if (length(eye_col) > 1 | length(pat_col) > 1) {
    stop("Patient and/or eye column(s) are not uniquely identified.", call. = FALSE)
  }

  n_pat <- length(unique(x[[pat_col]]))

  if (length(eye_col) == 1) {
    eye_int <- suppressWarnings(unique(as.integer(x[[eye_col]])))

    if (length(eye_int) > 2) {
      stop("There are more than 2 numeric codes for eyes. Clean your data", call. = FALSE)
    } else if (length(eye_int) == 2) {
      if(!(all(eye_int %in% 0:1) | all(eye_int %in% 1:2))){
        stop("Eyes are numerically coded, but coding is not 0/1 or 1/2.
           Please change the codes for eyes.", call. = FALSE)
      } else {
        warning("Eyes are numerically coded with 0/1 or 1/2.
              Default to r = 0 / l = 1 or r = 1 / l = 2")
      }
    } else if (is.na(eye_col)){
      if (!all(tolower(unique(x[[eye_col]])) %in% c(NA, "r", "l", "re", "le", "od", "os"))) {
        stop("Eyes not coded clearly.
          Must be either of c(\"r\", \"l\", \"re\", \"le\", \"od\", \"os\") - any cases allowed!", call. = FALSE)
      }
    }
    if (sum(is.na(x[[eye_col]]) > 0)) {
      warning("There are observations where the eyes are not identified (NA)", call. = FALSE)
    }
    eye_tab <- table(unique(x[, c(pat_col, eye_col)]))
    n_eyes <- sum(colSums(eye_tab))
    if(all(eye_int %in% 0:1)){
      n_r <- colSums(eye_tab[, colnames(eye_tab) == 0, drop = FALSE])
      n_l <- colSums(eye_tab[, colnames(eye_tab) == 1, drop = FALSE])
    } else if (all(eye_int %in% 1:2)){
      n_r <- colSums(eye_tab[, colnames(eye_tab) == 1, drop = FALSE])
      n_l <- colSums(eye_tab[, colnames(eye_tab) == 2, drop = FALSE])
    } else {
      tab_r <- eye_tab[, tolower(colnames(eye_tab)) %in% c("r", "re", "od"), drop = FALSE]
      if (ncol(tab_r) > 1) {
        warning(paste0(
          "Found several ways to code for right eyes (",
          paste(colnames(tab_r), collapse = ","), ") - suggest clean your data"
        ),
        call. = FALSE
        )
      }
      n_r <- sum(colSums(tab_r))

      tab_l <- eye_tab[, tolower(colnames(eye_tab)) %in% c("l", "le", "os"), drop = FALSE]
      if (ncol(tab_l) > 1) {
        warning(paste0(
          "Found several ways to code for left eyes (",
          paste(colnames(tab_l), collapse = ","), ") - suggest clean your data"
        ),
        call. = FALSE
        )
      }

      n_l <- sum(colSums(tab_l))
    }

    if (text) {
      return(paste(n_eyes, "eyes of", n_pat, "patients"))
    }
    return(c(patients = n_pat, eyes = n_eyes, right = n_r, left = n_l))
  } else {
    return(c(patients = n_pat))
  }
}

#' count_eyes
#' @rdname eyes
#' @export
counteyes <- eyes

#' count_eyes
#' @rdname eyes
#' @export
count_eyes <- eyes

#' get_age
#' @description calculates age in years, either as duration or as period
#' Slight modification of a function [posted on stackoverflow by user Moody_Mudskipper](https://stackoverflow.com/a/47529507/7941188)
#' @param from_date start date
#' @param to_date end date
#' @param period default FALSE: output as a duration. If TRUE, output as a period
#' @family convenience functions
#' @export

get_age <- function(from_date, to_date = lubridate::now(), period = FALSE){
  if(!require('lubridate'))
    stop('Please install the lubridate package')
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (period) { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))
  age
  } else { age <- lubridate::interval(start = from_date, end = to_date)/lubridate::dyears(1)
  age
  }
}


#' show_stats
#' @description Shows commonly used summary statistics
#' @param x either vector or list of vectors. Data frames supported.
#' Character vectors (or character columns) will be removed.
#' @param dec how many decimals are displayed
#' @param rownames if FALSE, column with variable names will be created
#' @importFrom purrr partial
#' @family stats functions
#' @return named vector (for vector) or data frame (for list)
#' @examples
#' x = y = z = c(rnorm(20), NA)
#'
#' # named or unnamed list
#' mylist <- list(x = x, y = y, z = z)
#' show_stats(mylist)
#' # with a data frame
#' mydf <- data.frame(x, y, z)
#' show_stats(mydf)
#' #If aggregation by group, split the data frame first
#' mydf2 <- data.frame(group = rep(letters[1:2], each = 42), x, y, z)
#' lapply(split(mydf2, mydf2$group), show_stats, rownames = FALSE)
#' @export

show_stats <- function(x, dec = 1, rownames = TRUE) {
  if(!require('purrr'))
    stop('Please install the purrr package')
  funs <- list(
    mean = purrr::partial(mean, na.rm = T),
    sd = purrr::partial(sd, na.rm = T),
    n = length,
    median = purrr::partial(median, na.rm = TRUE),
    min = purrr::partial(min, na.rm = TRUE),
    max = purrr::partial(max, na.rm = TRUE)
  )

  if (is.atomic(x) == TRUE) {
    r1 <- lapply(funs, function(f) f(x))

    if(!rownames){
      cbind(var = rownames(unlist(r1)), data.frame(unlist(r1), row.names=NULL))
    } else {
      unlist(r1)
    }
  } else if (typeof(x) == "list") {
    x_num <- Filter(is.numeric, x)
    if(!identical(x, x_num)) warning("Character columns or list elements removed")
    result <- lapply(funs, mapply, x_num)
    list_res <- lapply(result, function(y) round(y, digits = dec))
    if(!rownames){
      cbind(var = rownames(data.frame(list_res)), data.frame(data.frame(list_res), row.names=NULL))
    } else {
      data.frame(list_res)
    }

  }
}

#' anonymize
#' @name anonymize
#' @aliases anonymise
#' @description The function will anonymize your data.
#' Modified from [this](https://stackoverflow.com/a/10455729/7941188) and [this](https://stackoverflow.com/a/10458688/7941188)
#' post on Stackoverflow from user Etienne Low-Décarie and Gavin Simpson
#' @param df data frame to anonymise
#' @param simple_names logical, column names will be converted to letters. If FALSE, colString will be used for renaming
#' @param replace_rownames if TRUE, replacement with string from rowString argument
#' @family convenience functions
#' @return Data frame
#' @export

anonymize <- function(df, simple_names = TRUE, replace_rownames = FALSE, colString = "var", rowString = "sample") {
  level.id.df <- function(df) {
    if (simple_names) {
      if (length(df) > 26) {
        LETTERS <- replicate(floor(length(df) / 26), {
          LETTERS <- c(LETTERS, paste(LETTERS, LETTERS, sep = ""))
        })
      }
      colnames(df) <- paste(LETTERS[1:length(df)])
    } else {
      colnames(df) <- paste(colString, seq_len(ncol(df)), sep = "")
    }
    level.id <- function(i) {
      if (class(df[, i]) == "factor" | class(df[, i]) == "character") {
        column <- paste(names(df)[i], as.numeric(as.factor(df[, i])), sep = "_")
      } else if (is.numeric(df[, i])) {
        column <- round(scale(df[, i]), 2)
      } else {
        column <- df[, i]
      }
      return(column)
    }
    DF <- data.frame(sapply(seq_along(df), level.id))
    colnames(DF) <- colnames(df)
    return(DF)
  }
  df <- level.id.df(df)
  if (replace_rownames) {
    rownames(df) <- paste(rowString, seq_len(nrow(df)), sep = "")
  }
  return(df)
}

#' anonymise
#' @rdname anonymize
#' @export
anonymise <- anonymize

#' csv
#' @description wrapper around write.csv with default 'row.names = FALSE'.
#' Will use the name of the data frame for the generated .csv file.
#' @name csv
#' @param x data frame
#' @param name Filename. Default: Name of dataframe to save as csv. Or character string (.csv extension added automatically)
#' @family convenience functions
#' @export

csv <- function(x, name = deparse(substitute(x))) {
  file = paste0(name, '.csv')
  write.csv(x, file, row.names = F)
}

#' Probability contours
#' @description calculates 2d probability contours for use in ggplot2.
#' Modified from [this post](https://stackoverflow.com/a/59173290/7941188) by user crsh
#' @name prob_contour
#' @param data data frame x and y coordinates
#' @param x column with x coordinates (default first column)
#' @param y column with y coordinates (default second column)
#' @param prob probability to be estimated
#' @param n passed to [MASS::kde2d]
#' @param ... further parameters passed to [MASS::kde2d]
#' @family stats functions
#' @export
#' @examples
#'
#' library(ggplot2)
#' set.seed(1)
#' n=100
#' foo <- data.frame(x=rnorm(n, 0, 1), y=rnorm(n, 0, 1))
#'
#' df_contours <- dplyr::bind_rows(
#'   purrr::map(seq(0.2, 0.8, 0.2), function(p) prob_contour(foo, prob = p))
#' )
#'
#' ggplot() +
#'   geom_point(data = foo, aes(x = x, y = y)) +
#'   geom_polygon(data = df_contours, aes(x = x, y = y, color = prob), fill = NA) +
#'   scale_color_brewer(name = "Probs", palette = "Set1")

prob_contour <- function(data, x = NULL, y = NULL, n = 50, prob = 0.95, ...) {
  if (ncol(data) > 2 & missing(x) & missing(y)) {
    warning("Data frame has more than two columns. Default to first two columns", call. = FALSE)
  } else if (ncol(data) > 2 & missing(x)) {
    warning("Data frame has more than two columns and x not specified.
            Default x to column 1", call. = FALSE)
  } else if (ncol(data) > 2 & missing(y)) {
    warning("Data frame has more than two columns and y not specified.
            Default y to column 2", call. = FALSE)
  }

  if (missing(x)) x <- 1L
  if (missing(y)) y <- 2L

  post1 <- MASS::kde2d(data[[x]], data[[y]], n = n, ...)

  dx <- diff(post1$x[1:2])
  dy <- diff(post1$y[1:2])
  sz <- sort(post1$z)
  c1 <- cumsum(sz) * dx * dy

  levels <- sapply(prob, function(x) {
    approx(c1, sz, xout = 1 - x)$y
  })

  df <- as.data.frame(grDevices::contourLines(post1$x, post1$y, post1$z, levels = levels))
  df$x <- round(df$x, 3)
  df$y <- round(df$y, 3)
  df$level <- round(df$level, 2)
  df$prob <- as.character(prob)

  df
}

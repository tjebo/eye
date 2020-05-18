

#' age
#' @name age
#' @description calculates age in years, either as duration or as period
#' Slight modification of a function [posted on stackoverflow by user Moody_Mudskipper](https://stackoverflow.com/a/47529507/7941188)
#' @param from_date start date
#' @param to_date end date
#' @param period default FALSE: output as a duration. If TRUE, output as a period
#' @family convenience functions
#' @examples
#' age("1984-10-16")
#'
#' dob <-  c("1984-10-16", "2000-01-01")
#' test_date <-  as.Date(dob) + c(15000, 20000)
#' age(dob, test_date)
#' @export

age <- function(from_date, to_date = lubridate::now(), period = FALSE, dec = 1){
  if(!require('lubridate'))
    stop('Please install the lubridate package')
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (period) { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))
  round(age, dec)
  } else { age <- lubridate::interval(start = from_date, end = to_date)/lubridate::dyears(1)
  round(age, dec)
  }
}


#' see
#' @description Shows commonly used summary statistics
#' @param x either vector or list of vectors. Data frames supported.
#' Character vectors (or character columns) will be removed.
#' @param dec how many decimals are displayed
#' @param dataframe returning list if FALSE
#' @importFrom purrr partial
#' @family stats functions
#' @return data frame (default) or list
#' @examples
#' x = y = z = c(rnorm(20), NA)
#'
#' # named or unnamed list
#' mylist <- list(x = x, y = y, z = z)
#' see(mylist)
#' # with a data frame
#' mydf <- data.frame(x, y, z)
#' see(mydf)
#' #If aggregation by group, split the data frame first
#' mydf2 <- data.frame(group = rep(letters[1:2], each = 42), x, y, z)
#' lapply(split(mydf2, mydf2$group), see, rownames = FALSE)
#' @export

see <- function(x, dec = 1, dataframe = TRUE) {
  funs <- list(
    mean = function(x) mean(x, na.rm = TRUE),
    sd = function(x) sd(x, na.rm = TRUE),
    n = length,
    median = function(x) median(x, na.rm = TRUE),
    min = function(x) min(x, na.rm = TRUE),
    max = function(x) max(x, na.rm = TRUE)
  )
  if (is.atomic(x)) {
    list_res <- lapply(funs, function(f) round(f(x), dec))

  } else if (typeof(x) == "list") {
    x_num <- Filter(is.numeric, x)
    if (!identical(x, x_num)) {
      warning("Character columns or list elements removed", call. = FALSE)
    }
    result <- lapply(funs, mapply, x_num)
    list_res <- lapply(result, function(y) round(y, digits = dec))
  }
  if (!dataframe) {
    list_res
  } else {
    data.frame(list_res)
  }
}

#' csv
#' @description wrapper around write.csv with default 'row.names = FALSE'.
#' Will use the name of the data frame for the generated .csv file.
#' @name csv
#' @param x data frame
#' @param name Filename. Default: Name of dataframe to save as csv. Or character string (.csv extension added automatically)
#' @family convenience functions
#' @examples
#' \dontrun{
#' csv(amd)
#' }
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

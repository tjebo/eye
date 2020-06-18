#' print eye classes
#' @name print_methods
#' @description S3 methods for VA classes "snellen", "logmar" and "etdrs".
#' **snellen** is always also a character class- because it is more categorical
#' than continuous. **logmar** and **etdrs** are both numerics
#' (logMAR is double, etdrs is integer).
#' @param x an object of a class from the eye package (VA class or blink class)
#' @param ... arguments passed to [print.default]
#' @return No return value, called for side effects (printing)
#' @export
print.snellen <- function(x, ...) {
  print.default(as.character(x))
}
#' print.logmar
#' @rdname print_methods
#' @export
print.logmar <- function(x, ...) {
  print.default(as.numeric(x))
}
#' print.etdrs
#' @rdname print_methods
#' @export
print.etdrs <- function(x, ...) {
  print.default(as.integer(x))
}

#' @rdname print_methods
#' @description S3 methods for blink class
#' @param x object of class "blink"
#' @param ... arguments passed to [print.default]
#' @export
#' @importFrom cli cli_h1
#' @importFrom cli cli_h3
#' @importFrom cli rule
print.blink <- function(x, ...) {
  cli::cli_h1("blink")
  cat(paste0(cli::rule(line = 2, left = "Data", width = 40),
             "\n"))
  print(x$data)
  cat(paste0("\n", cli::rule(line = 2, left = "Count of patient and eyes", width = 40),
             "\n"))
  print(x$count)
  cat(paste0("\n",
             cli::rule(line = 2, left = "Visual acuity", width = 40),
             "\n"))

  cli::cli_h3("$VA_total (all eyes)")
  print(x$VA_total)

  cli::cli_h3("$VA_eyes (right and left eyes)")
  print(x$VA_eyes)

  cat(paste0("\n",
             cli::rule(line = 2, left = "Intraocular pressure", width = 40),
             "\n"))

  cli::cli_h3("$IOP_total (all eyes)")
  print(x$IOP_total)

  cli::cli_h3("$IOP_eyes (right and left eyes)")
  print(x$IOP_eyes)
}

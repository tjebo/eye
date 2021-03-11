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
#' @description S3 methods for class blink
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

#' @rdname print_methods
#' @description S3 methods for class eyes
#' @param x object of class "eyes"
#' @param ... arguments passed to [print.default]
#' @export
#' @importFrom cli rule
print.eyes <- function(x, ...) {
  cat(paste0(cli::rule(line = 2, left = "Counts", width = 25),
             "\n"))
  print(unlist(x))
}
#' @rdname print_methods
#' @description S3 methods for class eyes_details
#' @param x object of class "eyes_details"
#' @param show how many subjects to be shown before printing the footnote
#' @param ... arguments passed to [print.default]
#' @export
#' @importFrom cli rule
#' @importFrom utils head
print.eyes_details <- function(x, show = 6, ...) {
  cat(paste0(cli::rule(line = 2, left = "$counts", width = 45),
             "\n"))
  print(unlist(x$counts))
  cat(paste0("\n", cli::rule(line = 2, left = "$id", width = 45),
             "\n"))
  print_obj <- lapply(x$id, head)
  ls_len <- lengths(x$id)
  lapply(1:3, function(i){
    if(ls_len[i] > show){
      cat(paste0("$", names(print_obj)[i], "\n"))
      print(print_obj[[i]])
      newfoot <- paste0("with " , ls_len[i] - show, " more subjects\n\n")
      cat(myfooter(newfoot))
    } else{
      print(print_obj[i])
    }
  })
}

#' @keywords internal
#' @importFrom pillar style_subtle
#' @importFrom cli symbol
myfooter <- function(x) {
  footer <- paste0(cli::symbol$ellipsis, " ", x)
  pillar::style_subtle(paste("#", footer))
}


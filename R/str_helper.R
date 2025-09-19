#' List of codes
#' @name eye_codes
#' @details
#' - r right eyes
#' - l left eyes
#' - iop IOP codes
#' - va VA codes
#' - method VA methods
#' - id patient column codes
#' - quali quali VA codes
#' @keywords internal
eye_codes <- list(
  right = c("r", "re", "od", "right"),
  left = c("l", "le", "os", "left"),
  both = c("b", "both", "ou", "be"),
  iop = c("iop", "gat", "nct"),
  iop_partial = c("pressure"),
  va = c("va", "bcva"),
  va_method = c("etdrs", "snellen", "logmar"),
  va_partial = c("acuit"),
  id = c("pat","id"),
  eye = c("eye", "eyes"),
  nlp = c("nlp", "no light perception", "no light", "no perception of light", "npl"),
  lp = c( "lp", "light perception", "perception of light", "pl"),
  hm = c("hm",  "handmotion", "hand movement", "hand movements"),
  cf = c("cf", "counting finger", "counting fingers", "finger count", "count fingers")
)
#' Set list of codes
#' @name set_eye_strings
#' @description This sets the list of codes used throughout the eye package for
#'   the coding of all kind of stuff. If you want to change recognized codes,
#'   this is the place to do it. See examples below how to easily overwrite it.
#'   It is important that you must pass them as a character vector!
#'
#'   **cases are always ignored**, so you don't need to worry about this bit.
#' @param right right eyes
#' @param left left eyes
#' @param both both eyes
#' @param iop IOP codes
#' @param iop_partial partial strings used to find IOP columns
#' @param va VA codes
#' @param va_method VA methods (used to recognize VA columns -
#'   when those strings occur "fully", i.e., not as part of sth else)
#' @param va_partial Also used to find VA columns - looking for partial strings
#' @param id patient column codes
#' @param eye eye column codes
#' @param nlp VA values recognised as "No light perception"
#' @param lp VA values recognised as "light perception"
#' @param hm VA values recognised as "hand movement"
#' @param cf VA values recognised as "count fingers"
#'
#' @param ... currently not used, but might be needed in the future
#' @details Beware, setting the recognised strings will fully overwrite
#' previously recognised ones. If you want to keep all, you need
#' to write them all out.
#' @section Restoring the defaults:
#' To restore the defaults, simply call set_eye_strings empty
#' @examples
#' # To expand recognized codes for eyes, e.g. if you want to use French names
#' set_eye_strings(right = c("droit", "od"), left = c("gauche", "og"))
#'
#' # To restore the defaults, simply call set_eye_strings empty
#' set_eye_strings()
#' @importFrom utils assignInNamespace
#' @export
set_eye_strings <- function(
  right = c("r", "re", "od", "right"),
  left = c("l", "le", "os", "left"),
  both = c("b", "both", "ou"),
  iop = c("iop", "gat", "nct"),
  iop_partial = c("pressure"),
  va = c("va", "bcva"),
  va_method = c("etdrs", "snellen", "logmar"),
  va_partial = c("acuit"),
  id = c("pat","id"),
  eye = c("eye", "eyes"),
  nlp = c("nlp", "no light perception", "no light", "no perception of light", "npl"),
  lp = c( "lp", "light perception", "perception of light", "pl"),
  hm = c("hm",  "handmotion", "hand movement", "hand movements"),
  cf = c("cf", "counting finger", "counting fingers", "finger count",
         "count fingers"),
  ...){
  new_eyecodes <- c(as.list(environment()), list(...))
  utils::assignInMyNamespace("eye_codes", new_eyecodes)
}

#' sort substrings
#' @description unify code for substrings and arrange, after tokenizing
#' @name sort_substr
#' @param x vector of strings
#' @param list_substr list of substrings to match against and sort -
#'   the order in the list defines the resulting order in the string
#'   Should be names list - the names will be the codes to which the
#'   tokens will be matched against.
#' @keywords internal
#' @family string matching functions
sort_substr <- function(x, list_substr) {
  if(!inherits(list_substr, "list")){
    stop("list_substr needs to be a named list", call. = FALSE)
  }
  lookups <- data.frame(match = rep(names(list_substr), lengths(list_substr)),
                        token = unlist(list_substr))

  l <- strsplit(x, "_", fixed = TRUE)
  DF <- data.frame(id = rep(seq_along(l), lengths(l)), token = unlist(l))
  match_token <- lookups$match[match(DF$token, lookups$token)]
  DF$match <- ifelse(is.na(match_token), DF$token, match_token)
  rest_token <- base::setdiff(DF$match, names(list_substr))
  DF$match <- factor(DF$match, levels = c(names(list_substr), rest_token))
  DF <- DF[with(DF, order(id, match)), ]

  out <- vapply(split(DF$match, DF$id),
                paste, collapse = "_",
                FUN.VALUE = character(1),
                USE.NAMES = FALSE)
  out
}

#' String search helper
#' @name str_search
#' @param needle vector of strings to look for - accepts regular expressions
#' @param haystack object in which to look for the needle
#' @description **whole_str** finds haystack with "whole needles"
#'   with any non character as boundaries
#' @keywords internal
#' @family string matching functions
whole_str <- function(haystack, needle) {
  reg <- paste0("(?<![a-z])(", paste(tolower(needle), collapse = "|"), ")(?![a-z])")
  haystack[grepl(reg, tolower(haystack), perl = TRUE)]
}

#' part_str
#' @description **part_str** finds haystacks that match any of the needles
#' @rdname str_search
part_str <- function(haystack, needle) {
  haystack[grepl(paste(tolower(needle), collapse = "|"),
    tolower(haystack),
    perl = TRUE
  )]
}

#' @description **both_str** will find haystacks that contain both needles
#' @rdname str_search
both_str <- function(haystack, needle) {
  if(length(needle)!=2){
    stop("needle needs to be of length 2")
  }
  reg <- paste0("(", needle[1],".*", needle[2], "|",
                needle[2], ".*", needle[1], ")")
  haystack[grepl(reg, tolower(haystack), perl = TRUE)]
}

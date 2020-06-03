#' Set string codes
#' @name set_codes
#' @description set the eye strings looked for in the data
#' @family string matching functions
set_codes <- function(
  r = c("r", "re", "od", "right"),
  l = c("l", "le", "os", "left"),
  iop = c("iop", "gat", "nct"),
  va = c("va", "bcva"),
  method = c("etdrs", "snellen", "logmar"),
  id = c("pat","id"),
  quali = c("NLP", "LP", "HM", "CF"),
  vaPart = "acuit", ...){
  c(as.list(environment()), list(...))
}

#' sort substrings
#' @description unify code for substrings and arrange, after tokenizing
#' @name sort_substr
#' @param x vector of strings
#' @param list_substr list of substrings to match against and sort -
#'   the order in the list defines the resulting order in the string
#'   Should be names list - the names will be the codes to which the
#'   tokens will be matched agains.
#' @examples
#' x <- c("id", "IOP_r", "iop_l", "Va_r", "va_l", "iop_r_post", "iop_l_post")
#' sort_substr(x, list(r = "r", l = "l", iop = "iop", va = "va"))
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


#' String search func facs
#' @name str_func_facs
#' @param x vector to search
#' @param string vector of strings to look for
#' @description internal function factory for functions
#'   to match "whole_str string" with any non character as boundaries
#'   accepts string as regular expression.
#' @family string matching functions
#' @examples
#' whole_str("eye")(amd)
#'
whole_str <- function(string) {
  reg <- paste0("(?<![a-z])(", paste(tolower(string), collapse = "|"), ")(?![a-z])")
  function(x) {
    x[grepl(reg, tolower(x), perl = TRUE)]
  }
}

#' part_str
#' @rdname str_func_facs
#' @examples
#' part_str(c("pat", "Id")(amd)

part_str <- function(string) {
  function(x) {
    x[grepl(paste(tolower(string), collapse = "|"), tolower(x), perl = TRUE)]
  }
}
#' both_str
#' @rdname str_func_facs
#' @param string_vec vector of two strings
#' @examples
#' both_str(c("pat","id"))(c("patid", "id", "pat"))

both_str <- function(string_vec) {
  if(length(string_vec)!=2){
    stop("string_vec needs to be of length 2")
  }
  function(x) {
  reg <- paste0("(", string_vec[1],".*", string_vec[2], "|",
                string_vec[2], ".*", string_vec[1], ")")
    x[grepl(reg, tolower(x), perl = TRUE)]
  }
}


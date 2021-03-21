#' Count patients and eyes
#' @name eyes
#' @description Counts number of subjects and right and left eyes. Columns are
#'     guessed.
#' @param x required. (data frame)
#' @param id_col Subject identifying column, passed as (quoted) character
#'   Can also be abbreviated to "id" as per partial matching
#' @param eye_col Eye identifying column, passed as (quoted) character.
#'   Can also be abbreviated to "eye" as per partial matching
#' @param dropunknown introduces NA for values not recognized by [recodeye]
#' @param details if TRUE, will add information about which and how many
#'   subjects have only one eye or both included, and provide a list of
#'   subject IDs for each
#' @section Column guessing:
#' **id_col** and **eye_col** arguments overrule the name guessing for
#' the respective columns (here, cases need to match). Both arguments can be
#' abbreviated (*id* or *eye*) as per partial argument name matching.
#'
#' For any below, **cases are always ignored** (column names can be in upper or
#'    lower case, as you please)
#'
#' **patient ID columns**:
#' - First, `eyes` is looking for names that contain both strings
#' "pat" and "id" (the order doesn't matter) -
#'   you can change this with [set_eye_strings]
#' - Next, it will look for columns that are plainly called "ID"
#' - Last, it will search for all names that contain either "pat"
#' or "id"
#'
#' **eye column**:
#' - `eyes` primarily looks for columns called either "eye" or "eyes",
#' (you can change this with [set_eye_strings])
#' and if they are not present, columns containing string "eye"
#' (e.g., EyeName will be recognized)
#'
#' @section Eye coding:
#' The following codes are recognized: (change this with [set_eye_strings])
#'
#' - integer coding 0:1 and 1:2, right eye being the lower number.
#' - right eyes: c("r", "re", "od", "right") and
#' - left eyes: c("l", "le", "os", "left") and
#' - both eyes: c("b", "both", "ou")
#'
#' If your eye column contains other values, they will be dropped to
#' NA (dropunknown) or kept (and then only patients will be counted, because
#' coding remains unclear). Recommend then to recode with [recodeye]
#' @return List (of class "eyes" with count of patients and eyes
#'   if "details = TRUE", an list of class "eyes_details" will be returned
#' @family eye core functions
#' @importFrom purrr quietly
#' @examples
#' library(eyedata)
#' eyes(amd2)
#'
#' ## If you code your eyes with different strings,
#' ## e.g., because you are using a different language,
#' ## you can change this either with `set_eye_strings`
#' set_eye_strings(right = c("droit", "od"), left = c("gauche", "og"))
#'
#' ## restore defaults with
#' set_eye_strings()
#' @export
#'
eyes <- function(x, id_col = NULL, eye_col = NULL, dropunknown = TRUE, details = FALSE) {
  if (!inherits(x, "data.frame")) {
    stop("x must be a data frame", call. = FALSE)
  }
  if (nrow(x) < 1) {
    return("0 eyes of 0 subject")
  }
  if (is.null(id_col)) {
    id_col <- getElem_id(names(x))
  }

  if (length(id_col) != 1) {
    warning("Did not find the ID column - use argument \"id_col\"",
      call. = FALSE
    )
    return(NULL)
  }
  if (is.null(eye_col)) {
    eye_col <- getElem_eyecol(names(x))
  }
  if (length(eye_col) != 1) {
    message("Unclear which is the eye column. Counting id only.
Specify eye column with \"eye_col\" argument")
    res <- c(id = length(unique(x[[id_col]])))
  } else if (length(eye_col) == 1) {
    quiet_recode <- purrr::quietly(~ recodeye(x = .x, dropunknown = .y))
    recode_eye <- quiet_recode(.x = x[[eye_col]], .y = dropunknown)
    if (length(recode_eye$warnings) > 0) {
      message(recode_eye$warnings)
      return(c(id = length(unique(x[[id_col]]))))
    }
    if (length(recode_eye$messages) > 0) {
      message(gsub("\\\n", "", recode_eye$messages))
    }
    if(!details & any(unique(recode_eye$result) %in% "b")){
      message("Some rows contain information for both eyes. Correct?")
    }
    x[[eye_col]] <- recode_eye$result
    res <- count_eyes(x = x, id_col = id_col, eye_col = eye_col, details = details)
  }
  res
}

#' internal count
#' @name count_eyes
#' @param x object (data frame)
#' @param id_col patient column
#' @param eye_col eye column
#' @param details if TRUE, will get more information
#' @description `count_eyes()` is the internal counting function for [eyes]
#' @return Named integer vector with count of patients and eyes
#' @keywords internal
count_eyes <- function(x, id_col, eye_col, details = FALSE) {
  n_pat <- length(unique(x[[id_col]]))

  rl <- c("r", "l")
  x[[eye_col]] <- factor(x[[eye_col]], levels = c("b", rl))
  eye_tab <- table(unique(x[, c(id_col, eye_col)]))
  eye_tab[, rl][eye_tab[, "b"] == 1] <- 0
  out <- colSums(eye_tab)
  outfinal <- out[rl] + out["b"]
  n_eyes <- sum(outfinal)
if(!details){
  nr <- unname(outfinal["r"])
  nl <- unname(outfinal["l"])
  res <- list(id = n_pat, eyes = n_eyes, right = nr, left = nl)
  class(res) <- c("eyes", class(res))
  return(res)
} else {
  both <- eye_tab[, "b"] == 1 | rowSums(eye_tab) > 1
  r_only <- eye_tab[, "r"] == 1 & rowSums(eye_tab) == 1
  l_only <- eye_tab[, "l"] == 1 & rowSums(eye_tab) == 1
  nb <- sum(both)
  nr <- sum(r_only)
  nl <- sum(l_only)
  eyecount <- c(id = n_pat, eyes = n_eyes, right = nr, left = nl, both = nb)
  patid <- rownames(eye_tab)
  id <- list(right = patid[r_only], left = patid[l_only], both = patid[both])
  res <- list(counts = as.list(eyecount), id = id)
  class(res) <- c("eyes_details", class(res))
  return(res)
  }
}
#' @rdname eyes
#' @inheritParams eyes
#' @param ... passed to [eyes]
#' @inheritParams eyes_to_string
#' @section eyestr:
#'  eyestr creates a string which can be pasted into reports.
#'  It currently only supports "x eyes of n patient(s)" This is a limitation,
#'   but I guess in the vast majority  of cases will be "correct".
#'   To use for other categories (e.g., "people" or "participants"), use
#'   `eyes(...)[1]`
#' @return `eyestr`: Character string - can be directly pasted into reports
#' @examples
#' # Examples for the usage of eyestr
#' eyestr(amd2)
#'
#' set.seed(1)
#' ls_dat <-
#'   lapply(c(1, 12, 13),
#'     function(x) data.frame(id = as.character(1:x),
#'                            eye = sample(c("r", "l"), x, replace = TRUE)))
#'
#' lapply(ls_dat, eyestr, english = "small")
#' lapply(ls_dat, eyestr, english = "all")
#' lapply(ls_dat, eyestr, english = "all", caps = TRUE)
#' lapply(ls_dat, eyestr, english = "none")
#' lapply(ls_dat, eyestr, english = "none")
#' @export
eyestr <- function(x, ..., english = "small", caps = FALSE){
  res <- unlist(suppressMessages(eyes(x, ...)))
    res_str <- res[c("id", "eyes")]
    eyes_to_string(res_str[!is.na(res_str)], caps = caps,
                   english = english)
  }

#' Eye count to strings
#' @name eyes_to_string
#' @param x vector of one or two
#' @param english Which numbers to be written in plain english:
#'   choose "small" for numbers till 12, "all" (all numbers),
#'   or "none" (or any other string!) for none
#' @param caps if TRUE, first number will have capital first letter
#' @return Character string - can be directly pasted into reports
#' @keywords internal
#' @importFrom english english

eyes_to_string <- function(x, english = "small", caps = FALSE) {
  if (x[1] <= 1) patient <- "patient" else patient <- "patients"
  if (!is.na(x[2]) & x[2] <= 1) eye <- "eye" else eye <- "eyes"

  if (english == "small") {
    x[x <= 12] <- as.character(english::english(x[x <= 12]))
  } else if(english == "all"){
    x <- as.character(english::english(x))
  }

  if (length(x) == 1) {
    if(!caps) return(paste(x[1], patient))
    return(paste(tocapital(x[1]), patient))
  } else {
    if(!caps) return(paste(x[2], eye, "of", x[1], patient))
    return(paste(tocapital(x[2]), eye, "of", x[1], patient))
  }
}

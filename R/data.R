#' Real life data of patients with neovascular AMD
#' @docType data
#' @name amd
#' @description
#' A dataset containing anonymised real life human subjects data on eyes with
#' treatment naive neovascular age-related macular degeneration (AMD),
#' which underwent intravitreal anti_VEGF therapy with ranibizumab
#' and/or aflibercept.
#'
#' The data was collected in Moorfields Eye Hospital, London, UK.
#' (Information governance sign off Moorfields Eye Hospital 19/07/2018)
#'
#' @usage data("amd")
#' @format A data frame with 40764 rows and 7 variables:
#' \describe{
#'   \item{Id}{Anonymized patient identifier}
#'   \item{Eye}{Left or right eye of patient (0 = right, 1 = left)}
#'   \item{FollowupDays}{Days after date of first appointment (0 = first appointment)}
#'   \item{BaselineAge}{Age (years) at day of first appointment}
#'   \item{Gender}{Gender of patient (0 = male, 1 = female)}
#'   \item{VA_ETDRS_Letters}{Visual acuity in Early Treatment Diabetic Retinopathy Study letters}
#'   \item{InjectionNumber}{Current number of injection at appointment date}
#' }
#' @source \url{https://datadryad.org/stash/dataset/doi:10.5061/dryad.97r9289}
"amd"


#' Visual acuity conversion chart
#' @docType data
#' @name va_chart
#' @description Conversion between snellen, logMAR and ETDRS.
#' Snellen feet, meter and decimal supported. Three qualitative common
#' vision measures included (light perception, hand movement and counting fingers)
#' @usage data("va_chart")
#' @format A data frame with 40764 rows and 7 variables:
#' \describe{
#'   \item{snellen_ft}{snellen VA in feet}
#'   \item{snellen_m}{snellen VA in meter}
#'   \item{snellen_dec}{decimal snellen VA}
#'   \item{logMAR}{logMAR VA}
#'   \item{ETDRS}{VA in ETDRS letters}
#' }
#'
"va_chart"

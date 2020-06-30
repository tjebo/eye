#' Real life data of patients with neovascular AMD
#' @docType data
#' @name amd
#' @description
#' A dataset containing anonymized real life human subjects data on eyes with
#' treatment naive neovascular age-related macular degeneration (AMD),
#' which underwent intravitreal anti-VEGF therapy with ranibizumab
#' and/or aflibercept.
#' @details
#' The data was collected in Moorfields Eye Hospital, London, UK.
#' (Information governance sign off Moorfields Eye Hospital 19/07/2018)
#'
#' Data was accessed on the 25th May 2020
#' @section Missing values:
#' There are two missing visual acuity entries in this data set. They result
#' from data entry errors (ETDRS values above 100) in the original
#' medical health records. Unfortunately, the correct VA value could not be
#' retrieved and it was decided to assign missing values to those measurements.
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
#' vision measures included (light perception, hand movement and
#' counting fingers). Further details for conversion used can be found in
#' [va] and [va_methods]
#' @usage data("va_chart")
#' @format A data frame with 29 rows and 5 variables:
#' \describe{
#'   \item{snellen_ft}{snellen VA in feet}
#'   \item{snellen_m}{snellen VA in meter}
#'   \item{snellen_dec}{decimal snellen VA}
#'   \item{logmar}{logMAR VA}
#'   \item{etdrs}{VA in ETDRS letters}
#'   \item{quali}{VA categories}
#' }
#' @seealso
#' - This chart and VA conversion formulas are based on charts in
#' [Holladay et al.](https://doi.org/10.1016/j.jcrs.2004.01.014),
#' [Beck et al.](https://doi.org/10.1016/s0002-9394(02)01825-1), and
#' [Gregori et al.](https://doi.org/10.1097/iae.0b013e3181d87e04).
#'
#' Categories **(no) light perception**,  **counting fingers**
#' and **hand movements** are converted following
#' [Schulze-Bonsel et al.](https://doi.org/10.1167/iovs.05-0981)
#' and [Michael Bach's suggestions](https://michaelbach.de/sci/acuity.html)
#'
"va_chart"

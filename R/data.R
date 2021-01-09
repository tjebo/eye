
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
#'   \item{snellenft}{snellen VA in feet}
#'   \item{snellenm}{snellen VA in meter}
#'   \item{snellendec}{decimal snellen VA}
#'   \item{logmar}{logMAR VA}
#'   \item{etdrs}{VA in ETDRS letters}
#'   \item{quali}{VA categories}
#' }
#' @seealso
#' - This chart and VA conversion formulas are based on charts in
#' Holladay et al.\doi{10.1016/j.jcrs.2004.01.014}, Beck et al.
#' \doi{10.1016/s0002-9394(02)01825-1}{Beck et al.}, and
#' Gregori et al.\doi{10.1097/iae.0b013e3181d87e04}.
#'
#' Categories **(no) light perception**,  **counting fingers**
#' and **hand movements** are converted following
#' Schulze-Bonsel et al. (https://doi.org/10.1167/iovs.05-0981)
#' and [Michael Bach's suggestions](https://michaelbach.de/sci/acuity.html)
#'
"va_chart"

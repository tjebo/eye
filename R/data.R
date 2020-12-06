#' Real life data of patients with neovascular AMD
#' @docType data
#' @name amd2
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
#' @usage data("amd2")
#' @format A data frame (tibble) with 40764 rows and 7 variables:
#' \describe{
#'   \item{patID}{Anonymized patient identifier}
#'   \item{sex}{Sex of patient (m = male, f = female)}
#'   \item{age0}{Age (years) at day of first appointment}
#'   \item{eye}{Left or right eye of patient (r = right, l = left)}
#'   \item{time}{Time in days after date of first appointment (0 = first appointment)}
#'   \item{va}{Visual acuity in Early Treatment Diabetic Retinopathy Study letters}
#'   \item{inj_no}{Current number of injection at appointment date}
#'   }
#' @source \url{https://doi.org/10.5061/dryad.97r9289}
#' @seealso
#' Scientific article to which this data set was supplement:
#' \url{https://doi.org/10.1136/bmjopen-2018-027441}
"amd2"

#' Real life data of patients with diabetic macular edema
#' @docType data
#' @name dme
#' @description
#' A dataset containing anonymized real life human subjects data on eyes with
#' diabetic macular edema (DME), which underwent intravitreal
#' anti-VEGF therapy with ranibizumab and/or aflibercept.
#' Data was accessed on the 3rd July 2020.
#' @section Missing values:
#' There are 18 missing visual acuity entries in this data set. They result
#' from data entry errors (ETDRS values above 100) in the original
#' medical health records. Unfortunately, the correct VA value could not be
#' retrieved and it was decided to assign missing values to those measurements.
#' @section Study setting and design:
#' Study setting and design: This study is a retrospective cohort study of
#' diabetic patients treated for DMO by anti-VEGF at a tertiary referral center -
#' Moorfields Eye Hospital NHS Foundation Trust, London, UK.
#' Approval was granted by the Institutional Review Board of the hospital
#' (ROAD17/031) - Audit registration was completed (MEH-233).
#' The study complied with the Declaration of Helsinki and STROBE guidelines
#' for the reporting of cohort studies.
#' @section Data source:
#' All clinical information at Moorfields Eye Hospital is recorded within an
#' electronic medical record (EMR) application (OpenEyes Foundation, London, UK).
#' A SQL database (SQL Server Reporting Service, Microsoft Corporation, Richmond, USA)
#' containing all the information from the EMR is in place and regular updates
#' are performed overnight to keep the data warehouse up-to-date.
#' VA is reported in ETDRS letter score. The highest value (independent of
#' measurement method) available at each visit was chosen.
#' @section Participants:
#' A data-warehouse query for patients that received one IVI for DMO
#' (between March 2013 and October 2018) resulted in 3226 unique eyes from
#' 2368 patients. Exclusion criteria were those that:
#' (i) suffered from
#' macular oedema secondary to other conditions than diabetes;
#' (ii) under 18 years old;
#' (iii) received fewer than 3 IVI;
#' (iv) received bevacizumab, dexamethasone intravitreal implant,
#' or fluocinolone acetonide intravitreal implant; leaving 2614 eyes of 1964
#' patients taken forward for analysis.
#' @usage data("dme")
#' @format A data frame (tibble) with 40281 rows and 8 variables:
#' \describe{
#'   \item{patID}{Anonymized patient identifier}
#'   \item{sex}{Sex of patient (m = male, f = female)}
#'   \item{ageStrat}{Age (years) at day of first appointment, stratified.
#'        Factor! Labels are constructed using "(a,b]" interval notation, e.g., "(60,70]"
#'        means x > 60 & x<= 70}
#'   \item{ethnicity}{Ethnicity of patient following the categories from the
#'        UK ETHNIC CATEGORY CODE 2001. (asian = Asian or Asian British,
#'        white = White, black = Black or Black British,
#'        mixed = Mixed, other = Other ethnic group, unknown = Unknown)}
#'   \item{eye}{Left or right eye of patient (r = right, l = left)}
#'   \item{va}{Visual acuity in Early Treatment Diabetic Retinopathy Study (ETDRS) letters}
#'   \item{time}{Time in days following baseline i.e. injection number 1}
#'   \item{inj}{Injection given or not (TRUE = injection given, FALSE = no injection given)}
#'   }
#' @source \url{https://doi.org/10.5061/dryad.pzgmsbcfw}
#' @seealso
#' Scientific article to which this data set was supplement:
#' \url{https://doi.org/10.1038/s41433-020-1048-0}
"dme"

#' Twelve years neovascular AMD survival data
#' @docType data
#' @name amd
#' @description
#' To explore potential utility of survival analysis techniques for retrospective
#' clinical practice visual outcomes in a retrospective cohort study with
#' 12-year observation period.
#' @section Study setting and design:
#'     Of 10,744 eyes with neovascular AMD receiving anti-VEGF therapy between
#'     October 2008 and February 2020, 7802 eyes met study criteria
#'     (treatment-naïve, first-treated eyes starting anti-VEGF therapy).
#'     Institution:	 Moorfields Eye Clinic, London, UK
#'     Approval was granted by the Institutional Review Board of the hospital
#'     (ROAD17/031). The study complied with the Declaration of Helsinki
#' @section Data source:
#' All clinical information at Moorfields Eye Hospital is recorded within an
#' electronic medical record (EMR) application (OpenEyes Foundation, London, UK).
#' A SQL database (SQL Server Reporting Service, Microsoft Corporation, Richmond, USA)
#' containing all the information from the EMR is in place and regular updates
#' are performed overnight to keep the data warehouse up-to-date.
#' VA is reported in ETDRS letter score (and categories for ETDRS < 1).
#' The highest value (independent of measurement method)
#' available at each visit was chosen.
#' @section Data:
#' Types of data: de-identified human subjects data
#' Information governance authorised Moorfields Eye Clinic 19/07/2018.
#' age not provided as a continuous variable as in original analysis
#' to facilitate de-identification
#' @usage data("amd")
#' @format A data frame (tibble) with 6696 rows and 23 variables:
#' \describe{
#'   \item{patID}{anonymised patient number}
#'   \item{sex}{gender of patient (m = male, f = female)}
#'   \item{age}{age at initiation of anti-VEGF therapy (50-59 years,
#'       60-69 years, 70-79 years, 80 years and above)}
#'   \item{avdays_induc}{arithmetical average of interval between injections in induction
#'        phase in days}
#'   \item{ethnicity}{ethnicity of patient: asian (South East Asian), caucasian
#'       Afro Caribbean, Mixed, unknown_other)}
#'   \item{loaded}{induction phase was appropriately completed within 90 days (TRUE)
#'        or not (FALSE)}
#'   \item{time}{days following initiation of anti-VEGF therapy}
#'   \item{injgiven}{whether injection was given at appointment time or
#'       not (TRUE = injections given , FALSE = injection not given)}
#'   \item{va}{visual acuity at time point in early treatment diabetic
#'       retinopathy study letter score}
#'   \item{regimen}{anti-VEGF drug given is ranibizumab only or aflibercept only}
#'   \item{pre2013}{ anti-VEGF therapy initiated before October 2013 (TRUE) or after (FALSE)
#'       i.e. before or after the introduction of aflibercept, respectively}
#'   }
#' @source \url{https://doi.org/10.5061/dryad.nvx0k6dqg}
#' @seealso
#' Associated publication:
#' „Insights from survival analyses during 12 years of anti-vascular endothelial
#' growth factor therapy for neovascular age-related macular degeneration“
#'   \url{https://doi.org/10.1001/jamaophthalmol.2020.5044}
#'
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
#' \href{https://doi.org/10.1016/s0002-9394(02)01825-1}{Beck et al.}, and
#' [Gregori et al.](https://doi.org/10.1097/iae.0b013e3181d87e04).
#'
#' Categories **(no) light perception**,  **counting fingers**
#' and **hand movements** are converted following
#' [Schulze-Bonsel et al.](https://doi.org/10.1167/iovs.05-0981)
#' and [Michael Bach's suggestions](https://michaelbach.de/sci/acuity.html)
#'
"va_chart"


## code to prepare `DATASET` dataset goes here
amd <- readr::read_delim("data-raw/Moorfields_AMD_Database_1.csv", delim = ";")

usethis::use_data(amd, overwrite = TRUE)

###  va conversion chart
#Snellen converted to logMAR = -1 * log10(Snellen fraction).
snellen_ft <-
  c(NA, "20/16000", "20/4000", "20/2000", "20/800", "20/630", "20/500", "20/400", "20/320", "20/300", "20/250", "20/200",
    "20/160", "20/125","20/100","20/80","20/70", "20/63",
    "20/60", "20/50","20/40","20/32","20/30","20/25","20/20",
    "20/16","20/15", "20/13", "20/10")

snellen_m <- c(NA, "6/4800", "6/1200", "6/600", "6/240","6/190","6/150", "6/120", "6/96","6/90", "6/75", "6/60", "6/48", "6/38",
               "6/30", "6/24", "6/21", "6/19", "6/18", "6/15", "6/12", "6/9.6",
               "6/9", "6/7.5", "6/6", "6/5", "6/4.5", "6/4", "6/3")
dec_sn <- c(NA, with(read.table(text = snellen_ft[!is.na(snellen_ft)], sep = "/"), V1 / V2))
logMAR <- c(4.2, round(-1 * log10(dec_sn[!is.na(dec_sn)]), 2))
ETDRS <- round(85 + 50 * log10(dec_sn), 0)
quali <- c("LP", "HM", "CF", rep(NA, length(logMAR)-3))

va_chart <- data.frame(snellen_ft,
                             snellen_m,
                            snellen_dec = formatC(round(dec_sn, 3)),
                            logMAR = formatC(logMAR),
                            ETDRS = as.integer(ETDRS),
                            quali)
va_chart$ETDRS[c(2:4, 28)] <- c(NA, 0L, 2L, 95L)
va_chart$logMAR[c(12, 25, 28)] <- c("1.0", "0.0", "-0.2")
va_chart$snellen_dec[c(1, 17:19, 23, 25, 27:29)] <- c(NA, '0.29', '0.32', '0.33', '0.66', '1.0', '1.33', '1.5', '2.0')

usethis::use_data(va_chart, overwrite = TRUE)


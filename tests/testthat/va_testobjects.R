## objects for va() testing

xtry <- c(NA, "nlp", 1:2, 1.1, -1, "20/40", "4/6", "6/1000", 34)
logmar <- va_chart$logmar
snellenft <- va_chart$snellenft
snellenm <- va_chart$snellenm
snellendec <- va_chart$snellendec
etdrs <- va_chart$etdrs
etdrs_unplaus <- seq(-1,30,1)
logmar_unplaus <- seq(-4.4,2.0, 0.1)
snellen_unplaus <- letters[1:20]
quali <- va_chart$quali
quali_logmar <- c(logmar, quali)
quali_etdrs<- c(etdrs, quali)
quali_snellen_ft <- c(snellenft, quali)
quali_snellen_m <- c(snellenm, quali)
mixed_VA <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA", "eye", -0.4, "NLP", "PL")
mixed_VA1 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",  -0.4, "NLP", "PL")
mixed_VA2 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",   "NLP", "PL")
etdrs_logmar <- 0:1
etdrs_logmar2 <-  c(0.2, 0.1, 75)
etdrs_logmar_na <- c(0:1, NA)
snellen_logmar <- intersect(as.numeric(va_chart$snellendec), va_chart$logmar)
snellen_logmar_na <- c(snellen_logmar, NA)
va_vecNA <- c(NA, NA)
va_vec <- c("NLP", "LP", "HM", "CF", "6/60", "NA", NA, "20/40")
va_vec1 <- c("NPL", "NLP", "LP", "PL", "HM", "CF", "6/60", "20/200", "6/9", "20/40")
va_vec2 <- structure(c(3, 2.7, 2.3, 1.9, 1, 1, 0.18, 0.3), class = c("logmar",
                                                                     "va", "numeric"))
va_vec3 <- structure(c(0L, 0L, 0L, 2L, 35L, 35L, 75L, 70L), class = c("etdrs",
                                                                      "va", "integer"))
va_vec4 <- c(NA, "CF", "NA", "HM", "6/9", "6/6", "6/18", "6/12", "6/5",
             "6/24", "6/36", "6/60", "3/60")
va_vec5 <- c("20/200","20/200 + 3", "20/200+3", "20/200-4","20/200 -4")

va_vec6 <- c("6/9", "6/6", "6/5", "6/12", "6/7.5", "6/18", "PL", "6/4",
             "6/24", "6/36", "HM", "6/60", "EYE NOT PRESENT", "CF", "N/A",
             "6/2", "NPL", "NOT TESTED", "Not tested", "6/3", "Eye not present",
             "6/7")
va_vec7 <- structure(c(NA, "6/9", "6/6", "6/18", "6/12", "6/5",
                       "6/24", "6/36", "6/60", "3/60" ),class = c("snellen",
                                                                  "va", "character"))
va_vec8 <- structure(quali,class = c("quali", "va", "integer"))



# $va_vec (length 8)
# [1] "NLP" "LP"  "HM"  "CF"  "6/60""20/200" "6/9" "20/40"
# $va_vec1 (length 10)
# "NPL" "NLP" "LP"  "PL"  "HM"  "CF"  "6/60""20/200" "6/9""20/40"
# $va_vec2 (length 8)
# [1] 3.00 2.70 2.30 1.90 1.00 1.00 0.18 0.30
# $va_vec3 (length 8)
# [1]  0  0  0  2 35 35 75 70
# $va_vec4 (length 13)
# [1] NA  "CF""NA""HM""6/9"  "6/6"  "6/18" "6/12" "6/5"  "6/24" "6/36" "6/60" "3/60"
# $va_vec5 (length 5)
# [1] "20/200"  "20/200 + 3" "20/200+3""20/200-4""20/200 -4"

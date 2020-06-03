logmar <- va_chart$logMAR
snellen_ft <- va_chart$snellen_ft
snellen_m <- va_chart$snellen_m
snellen_dec <- va_chart$snellen_dec
etdrs <- va_chart$ETDRS
etdrs_unplaus <- seq(-1,30,1)
logmar_unplaus <- seq(-4.4,2.0, 0.1)
snellen_unplaus <- letters[1:20]
quali <- va_chart$quali
quali_logmar <- c(logmar, quali)
quali_snellen_ft <- c(snellen_ft, quali)
quali_snellen_m <- c(snellen_m, quali)
mixed_VA <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA", "eye", -0.4, "NLP", "PL")
mixed_VA1 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",  -0.4, "NLP", "PL")
mixed_VA2 <- c(32, "20/40",3, NA, "1.1", 0.2, -0.3, "NA",   "NLP", "PL")
etdrs_logmar <- 0:1
etdrs_logmar_na <- c(0:1, NA)
snellen_logmar <- eye:::inter_snelllog[c(2,4,5)]
snellen_logmar_na <- c(eye:::inter_snelllog[c(2,4,5)], NA)
na_vec <- c(NA, NA)
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

abraham <- read.csv("./data-raw/share_vision_df.csv")

